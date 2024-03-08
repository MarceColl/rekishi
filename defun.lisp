(in-package :rekishi)

(defmacro defun/r (name args &body body)
  "`DEFUN' for `REKISHI' objects.

`DEFUN/R' operates exactly like `DEFUN' but it saves the definition
to the local `REKISHI' database.
"
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
