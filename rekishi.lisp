(in-package :cl-user)
(defpackage rekishi
  (:use :cl :ironclad :babel :bedrock)
  (:export :defun/r))
(in-package :rekishi)

(define-condition rekishi-error (error) ())

(defun symbol-to-string (sym)
  (format nil "~s:~s" (package-name (symbol-package sym)) (symbol-name sym)))

(defun query (query-string &optional args)
  (let* ((q (dbi:prepare *connection* query-string))
	 (q (dbi:execute q args)))
    (dbi:fetch-all q)))

(defclass hashset (hash-table) ()
  (:metaclass structure-class))

(defparameter *loaded-objects* (make-hash-table))

(defmethod set-contains ((set hashset) item)
  (let ((val (gethash item set)))
    (if val t nil)))

(defmethod set-add ((set hashset) item)
  (setf (gethash item set) t))

(defmethod set-diff ((set-a hashset) (set-b hash-table))
  (let ((out-set (make-hash-table)))
    (loop for val in (alexandria:hash-table-keys set-a)
	  when (not (set-contains set-b val))
	  do (set-add out-set val))
    out-set))

(defmethod set-join ((set-a hashset) (set-b hash-table))
  (let ((out-set (make-hash-table)))
    (loop for val in (alexandria:hash-table-keys set-a)
	  do (set-add out-set val))
    (loop for val in (alexandria:hash-table-keys set-b)
	  do (set-add out-set val))
    out-set))

(defmethod set-intersect ((set-a hashset) (set-b hash-table))
  (let ((out-set (make-hash-table)))
    (loop for val in (alexandria:hash-table-keys set-a)
	  when (set-contains set-b val)
	    do (set-add out-set val))
    out-set))

(defun make-database ()
  (query "DROP INDEX IF EXISTS parent_index")
  (query "DROP TABLE IF EXISTS objects")
  (query "DROP TABLE IF EXISTS bindings")
  (query "
CREATE TABLE objects (
  hash TEXT,
  definition TEXT,
  documentation TEXT,
  dependencies TEXT,
  parent TEXT,
  children TEXT,
  mtime INTEGER,
  PRIMARY KEY(hash)
)")
  (query "
CREATE TABLE bindings (
  binding TEXT,
  package TEXT,
  object TEXT,
  FOREIGN KEY(object) REFERENCES objects (hash),
  PRIMARY KEY(binding, package)
)")
  (query " CREATE INDEX parent_index ON objects (parent)"))

(defun upsert-object (&key hash definition documentation parent)
  (query
   "INSERT INTO objects (hash, definition, documentation, parent, mtime)
    VALUES (?, ?, ?, ?, unixepoch())
    ON CONFLICT DO
    UPDATE SET parent = ?"
   (list hash (format nil "~s" definition) documentation parent parent)))

(defun load-bindings (&optional package)
  "Load bindings into their packages. If a package is given then only load bindings for that package."
  (let* ((query-arg (if package (list package)))
	 (query-string (if package
			   "
SELECT b.binding, b.package, o.definition
FROM bindings AS b
JOIN objects AS o ON (o.hash = b.object)
WHERE package = ?"
			   "
SELECT b.binding, b.package, o.definition
FROM bindings AS b
JOIN objects AS o ON (o.hash = b.object)"))
	 (bindings (query query-string query-arg)))
    (dolist (binding bindings)
      (load-binding (getf binding :|binding|)
		    (getf binding :|package|)
		    (getf binding :|definition|)))))

(defun load-binding (binding package definition)
  (let* ((package (find-package package))
	 (*package* package))
    (let* ((lambda-string (format nil "(lambda ~a)" definition))
	   (definition-sexpr (read-from-string lambda-string))
	   (compiled-fn (compile nil definition-sexpr))
	   (binding-sym (intern binding *package*)))
      (setf (symbol-function binding-sym)
	    compiled-fn)
      (format T "Loaded ~a (~a)~%" binding-sym (package-name (symbol-package binding-sym))))))

(defun hash-function (args s-expr)
  (let ((s-expr-to-octets (string-to-octets (format nil "~a ~a" args s-expr))))
    (byte-array-to-hex-string (digest-sequence :sha512 s-expr-to-octets))))

(defun/r short-hash (hash)
  (subseq hash 0 10))

(defun/r get-binding-object-hash (binding)
  (let ((row (car (query
		   "SELECT object FROM bindings WHERE binding = ? AND package = ?"
		   (list (symbol-name binding) (package-name (symbol-package binding)))))))
    (getf row :|object|)))

(defun/r get-binding-object (binding)
  (let ((row (car (query
		   "SELECT * FROM objects WHERE hash = (SELECT object FROM bindings WHERE binding = ? AND package = ?)"
		   (list (symbol-name binding) (package-name (symbol-package binding)))))))
    row))

(defmacro defun/r (name args &body body)
  "`DEFUN' for `REKISHI' objects."
  (multiple-value-bind (values rem-forms decls doc-string)
      (alexandria:parse-body body :documentation t)
    (declare (ignore rem-forms decls))
    (let ((curr-binding-object-hash (get-binding-object-hash name))
	  (new-function-hash (hash-function args values))
	  (compiled-function (compile nil `(lambda ,args ,@body))))
      (dbi:with-transaction *connection*
	(upsert-object :hash new-function-hash
		       :definition `(,args ,@values)
		       :documentation doc-string
		       :parent curr-binding-object-hash)
	(set-binding name compiled-function new-function-hash)))))

(defun find-history (sym)
  (let ((query-string "
WITH RECURSIVE ancestor(hash, parent, definition, mtime) AS (
   SELECT hash, parent, definition, mtime FROM objects
   WHERE hash = (SELECT object FROM bindings WHERE binding = ? AND package = ?)
   UNION
   SELECT o.hash, o.parent, o.definition, o.mtime
   FROM ancestor AS a, objects AS o
   WHERE a.parent = o.hash
   UNION
   SELECT o.hash, o.parent, o.definition, o.mtime
   FROM ancestor AS a, objects AS o
   WHERE o.parent = a.hash
   ORDER BY o.mtime DESC
)
SELECT o.*, o.hash = (SELECT object FROM bindings WHERE binding = ? AND package = ?) AS current_binding
FROM objects AS o JOIN ancestor USING (hash)
ORDER BY o.mtime DESC"))
    (query query-string (list (symbol-name sym) (package-name (symbol-package sym)) (symbol-name sym) (package-name (symbol-package sym))))))

(defun set-binding (binding compiled-function new-hash)
  (setf (symbol-function binding) compiled-function)
  (query "
INSERT INTO bindings (binding, package, object) VALUES (?, ?, ?)
ON CONFLICT DO UPDATE SET object = ?
  " (list (symbol-name binding) (package-name (symbol-package binding)) new-hash new-hash)))

(defun/r get-object-by-hash (hash)
  (let* ((hash-like-str (format nil "~a%" hash))
	 (row (car (query "SELECT * FROM objects WHERE hash LIKE ?" (list hash-like-str)))))
    row))

(defun/r compile-from-definition (definition)
  (let ((lambda-sexpr (read-from-string (format nil "(lambda ~s)" definition))))
    (compile nil lambda-sexpr)))

(defun/r prev (binding)
  (let* ((curr-object (get-binding-object binding))
	 (parent (getf curr-object :|parent|)))
    (aif (getf curr-object :|parent|)
	 (let* ((parent-object (get-object-by-hash it))
		(parent-definition (getf parent-object :|definition|))
		(lambda-sexpr (read-from-string (format nil "(lambda ~s)" parent-definition)))
		(compiled-function (compile nil lambda-sexpr)))
	   (set-binding binding compiled-function parent)))))

(defun/r next (sym)
  "Move forwards in history for the given symbol"
  (aif (get-object-by-name sym)
       (let ((childs (ro-children it)))
	 (case (length childs)
	   (0 (error "No children to go to"))
	   (1 (let ((child (car childs)))
		(setf (gethash sym *objects-by-name*) child)
		(set-binding sym child)))
	   (t (error "Too many children, choose one")))
	 (get-current-object-info sym))
       (error "No object by that symbol")))

(defun get-object-by-name (sym)
  (gethash sym *objects-by-name*))

(defun get-current-object-info (sym)
  (let ((current-object (get-object-by-name sym)))
    (get-object-info current-object)))

(defun get-object-info (obj)
  (let* ((hash (ro-hash obj))
	 (binding (ro-binding obj))
	 (definition (ro-definition obj))
	 (parent (ro-parent obj))
	 (children (ro-children obj)))
    (format T "~%~a (~a)~%~%~(~a~) => ~(~s~)~%~%  PREV: ~a~%  NEXT:~%~{  + ~a~%~}~%"
	    binding
	    (short-hash hash)
	    (car definition)
	    (cdr definition)
	    (and parent (short-hash (ro-hash parent)))
	    (and children (mapcar (lambda (c) (short-hash (ro-hash c))) children)))))

(defun print-history (history)
  (format T "(oldest)~%")
  (dolist (obj history)
    (get-object-info obj)
    (format T "        |~%")
    (format T "        V~%"))
  (format T "(newest)~%"))


(defun/r add-1 (a)
  (+ a a a 4))
