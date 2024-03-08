(asdf:defsystem #:rekishi
  :depends-on (:bedrock :alexandria :ironclad :babel :cl-dbi)
  :components ((:file "rekishi"))
  :in-order-to ((test-op (test-op :chobun/test))))

(asdf:defsystem #:rekishi/test
  :depends-on (:rekishi :parachute)
  :components ((:module "t"
		:components ((:file "rekishi"))))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :parachute :test-toplevel :rekishi/test)))
