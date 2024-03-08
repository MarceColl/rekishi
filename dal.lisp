(in-package :rekishi)

(defparameter *connection* nil)

(defun setup-connection ()
  (dbi:connect :sqlite :database-name "/home/marcecoll/rekishi.sqlite3"))

(defun query (query-string &optional args)
  "Helper to query the DB"
  (let* ((q (dbi:prepare *connection* query-string))
	 (q (dbi:execute q args)))
    (dbi:fetch-all q)))

(defun upsert-object (&key hash definition documentation parent)
  (query
   "INSERT INTO objects (hash, definition, documentation, parent, mtime)
    VALUES (?, ?, ?, ?, unixepoch())
    ON CONFLICT DO
    UPDATE SET parent = ?"
   (list hash (format nil "~s" definition) documentation parent parent)))

(defun get-binding-object-hash (binding)
  (let ((row (car (query
		   "SELECT object FROM bindings WHERE binding = ? AND package = ?"
		   (list (symbol-name binding) (package-name (symbol-package binding)))))))
    (getf row :|object|)))

(defun get-binding-object (binding)
  (let ((row (car (query
		   "
SELECT o.*, b.package, b.binding FROM objects AS o
JOIN bindings AS b ON (b.object = o.hash)
WHERE binding = ? AND package = ?"
		   (list (symbol-name binding) (package-name (symbol-package binding)))))))
    row))

(defun get-history (sym)
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
