(in-package :cl-user)
(defpackage rekishi
  (:use :cl :ironclad :babel :bedrock)
  (:export :defun))
(in-package :rekishi)

(define-condition rekishi-error (error) ())

(defun symbol-to-string (sym)
  (format nil "~s:~s" (package-name (symbol-package sym)) (symbol-name sym)))

(defparameter *loaded-objects* (make-hash-set))

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

(defun short-hash (hash)
  (subseq hash 0 10))

(defun get-object-by-hash (hash)
  (let* ((hash-like-str (format nil "~a%" hash))
	 (row (car (query "SELECT * FROM objects WHERE hash LIKE ?" (list hash-like-str)))))
    row))

(defun compile-from-definition (definition)
  (let ((lambda-sexpr (read-from-string (format nil "(lambda ~s)" definition))))
    (compile nil lambda-sexpr)))

(defun get-symbols ()
  (mapcar
   (lambda (row) (getf row :|binding|))
   (query "SELECT binding FROM bindings")))

(defun get-object-definition (binding)
  (let ((obj (get-binding-object binding)))
    (list binding
	  (getf obj :|package|)
	  (read-from-string (getf (get-binding-object binding) :|definition|)))))

(defun prev (binding)
  "Move backwards in history for the given binding"
  (let* ((curr-object (get-binding-object binding))
	 (parent (getf curr-object :|parent|)))
    (aif (getf curr-object :|parent|)
	 (let* ((parent-object (get-object-by-hash it))
		(parent-definition (getf parent-object :|definition|))
		(lambda-sexpr (read-from-string (format nil "(lambda ~s)" parent-definition)))
		(compiled-function (compile nil lambda-sexpr)))
	   (set-binding binding compiled-function parent)))))

(defun next (sym)
  "Move forwards in history for the given binding"
  (aif (get-binding-object sym)
       (let ((childs (ro-children it)))
	 (case (length childs)
	   (0 (error "No children to go to"))
	   (1 (let ((child (car childs)))
		(setf (gethash sym *objects-by-name*) child)
		(set-binding sym child)))
	   (t (error "Too many children, choose one")))
	 (get-current-object-info sym))
       (error "No object by that symbol")))

(defun add-1 (a)
  (+ a a a 4))
