
(defvar rekishi-mode-syntax-table nil
  "Syntax table for `rekishi-mode'.")

(define-derived-mode rekishi-mode lisp-mode "Rekishi"
  "Major mode for using the Rekishi common lisp development env.")

(defface rekishi-symbol-header
  '((t :foreground "black"
       :background "green"
       :weight bold
       :underline t))
  "Face for symbol headers"
  :group 'rekishi-mode)

(defun rekishi-remove-symbol ()
  (interactive)
  (let ((start (previous-single-char-property-change (point) 'rekishi-sym))
	(end (next-single-char-property-change (point) 'rekishi-sym)))
    (let ((inhibit-read-only t))
      (remove-text-properties start end '(read-only nil 'rekishi-sym nil 'rekishi-type nil))
      (delete-region start end))))

(defun rekishi-toggle-collapsive-section ()
  (interactive)
  (let* ((header-end (point-at-eol))
	 (content-start (1+ header-end))
	 (content-end (or (next-single-char-property-change content-start 'rekishi-sym) (point-max)))
	 (is-hidden (get-char-property content-start 'invisible)))
    (if is-hidden
	(remove-overlays content-start content-end 'invisible t)
      (let ((overlay (make-overlay content-start content-end)))
	(overlay-put overlay 'invisible t)))))

(defun rekishi--move-to-next-empty-space ()
  (let ((in-symbol (get-text-property (point) 'rekishi-sym)))
    (if in-symbol
	(let ((next-free-pos (next-single-char-property-change (point) 'rekishi-sym)))
	  (goto-char next-free-pos))
      (let ((prev-free-pos (previous-single-char-property-change (point) 'rekishi-sym)))
	(goto-char prev-free-pos)))))

(defun rekishi-insert-symbol-collapsible-section (name package definition)
  (interactive)
  (rekishi--move-to-next-empty-space)
  (newline)
  (newline)
  (let ((map (make-sparse-keymap))
	(start (point)))
    (define-key map (kbd "<tab>")
		'rekishi-toggle-collapsive-section)
    (insert (propertize (format "SYM: %s (%s)" name package)
			'read-only t
			'mouse-face 'highlight
			'font-lock-face 'font-lock-keyword-face
			'keymap map))
    (let ((inhibit-read-only t))
      (insert (propertize "      4 versions"
			  'read-only t
			  'mouse-face 'highlight
			  'font-lock-face '(:foreground "gray50")
			  'keymap map)))
    (let ((inhibit-read-only t))
      (forward-line-or-newline)
      (insert (propertize (format "(DEFUN %s %s\n  %s)" name (car definition) (cadr definition))
			'rekishi-type 'content))
      (put-text-property start (point) 'rekishi-sym name))
    (forward-line 3)))

(defun rekishi--get-current-symbol-span ()
  (interactive)
  (let* ((curr-sym (get-text-property (point) 'rekishi-sym))
	 (start (previous-single-char-property-change (point) 'rekishi-sym))
	 (end (next-single-char-property-change (point) 'rekishi-sym)))
    (cl-values start end)))

(defun rekishi-move-symbol-up ()
  (interactive)
  (cl-multiple-value-bind (start end)
      (rekishi--get-current-symbol-span)
    (let ((text (buffer-substring start end)))
      (delete-region start end)
      
    
  

(defun rekishi-move-symbol-down ()
  (interactive))

(defun forward-line-or-newline ()
  (interactive)
  (let ((curr-line (current-line)))
    (forward-line)
    (if (= curr-line (current-line))
	(newline))))

(defun rekishi-insert-symbol-definition ()
  (interactive)
  (let ((string "(get-symbols)"))
    (sly-eval-async `(slynk:eval-and-grab-output ,string)
      (lambda (result)
	(cl-destructuring-bind (output value) result
	  (let* ((symbols (car (read-from-string value)))
		 (user-selection (completing-read "Insert Symbol: " symbols))
		 (form-to-eval (format "(get-object-definition '%s)" user-selection)))
	    (sly-eval-async `(slynk:eval-and-grab-output ,form-to-eval)
	      (lambda (result)
		(cl-destructuring-bind (output value) result
		  (let* ((data (car (read-from-string value)))
			 (binding (car data))
			 (package (cadr data))
			 (def (caddr data)))
		    (rekishi-insert-symbol-collapsible-section binding package def)))))))))))

(defvar rekishi-mode-map nil "Keymap for `rekishi-mode'")

(progn
  (setq rekishi-mode-map (make-sparse-keymap))
  (define-key rekishi-mode-map (kbd "C-c C-a") 'rekishi-insert-symbol-definition)
  (define-key rekishi-mode-map (kbd "C-c C-d C-s") 'rekishi-remove-symbol)
  (define-key rekishi-mode-map (kbd "M-<up>") 'rekishi-move-symbol-up)
  (define-key rekishi-mode-map (kbd "M-<down>") 'rekishi-move-symbol-down))

(define-derived-mode rekishi-mode lisp-mode "Rekishi"
  "A mode for Rekishi"

  (use-local-map rekishi-mode-map))


(defun rekishi-insert-symbol-def (sym)
  (sly-interactive-eval "(get-symbols)"))
  

(provide 'rekishi-mode)
