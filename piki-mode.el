;;; piki-mode.el --- piki mode


;;; Commentary:
;; 
;; TODO about piki
;; copy from wiliki-mode.el


;;; Code:

(defcustom piki-mode-hook nil
  "Hook called in `piki-mode'."
  :group 'piki
  :type 'hook)

(defface piki-header-1-face
  '((t (:weight bold :height 1.3 :inherit variable-pitch)))
  "Face for Piki headers of level 1.")

(defface piki-header-2-face
  '((t (:weight bold :height 1.2 :inherit variable-pitch)))
  "Face for Piki headers of level 2.")

(defface piki-header-3-face
  '((t (:weight bold :height 1.1 :inherit variable-pitch)))
  "Face for Piki headers of level 3.")

(defface piki-header-4-face
  '((t (:weight bold :height 1.0 :inherit variable-pitch)))
  "Face for Piki headers of level 4.")

(defface piki-header-5-face
  '((t (:weight bold :height 0.9 :inherit variable-pitch)))
  "Face for Piki headers of level 5 and below.")

(defface piki-link-face
  '((((class color) (background light)) (:foreground "#551a8b" :bold t))
    (((class color) (background dark)) (:foreground "cyan" :bold t))
    (t (:bold t)))
  "Face for Piki links.")

(defface piki-table-border-face
  '((((class color)) (:foreground "blue4"))
    (t (:bold t)))
  "Face for Piki table border.")

(defface piki-table-rule-face
  '((((class color)) (:foreground "skyblue4"))
    (t (:bold t)))
  "Face for Piki table border.")

(defface piki-verbatim-face
  '((((class color) (background light)) (:foreground "OliveDrab"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (((type tty) (class color)) (:foreground "green"))
    (t (:italic t)))
  "Face for Piki verbatim texts.")

(defface piki-bold-face
  '((t (:bold t)))
  "Face for Piki bold texts.")

(defface piki-italic-face
  '((t (:italic t)))
  "Face for Piki italic texts.")

(defvar piki-header-1-face 'piki-header-1-face)
(defvar piki-header-2-face 'piki-header-2-face)
(defvar piki-header-3-face 'piki-header-3-face)
(defvar piki-header-4-face 'piki-header-4-face)
(defvar piki-header-5-face 'piki-header-5-face)
(defvar piki-link-face 'piki-link-face)
(defvar piki-table-border-face 'piki-table-border-face)
(defvar piki-table-rule-face 'piki-table-rule-face)
(defvar piki-verbatim-face 'piki-verbatim-face)
(defvar piki-bold-face 'piki-bold-face)
(defvar piki-italic-face 'piki-italic-face)

(defvar piki-font-lock-emphasis-regions nil)

;; TODO execute piki (after-save-hook?)

(defun piki-font-lock-init ()
  (set (make-local-variable 'piki-font-lock-emphasis-regions) nil)
  '((lambda (end)
      (setq piki-font-lock-emphasis-regions nil)
      nil)))

(defun piki-fontify-later (face)
  `(ignore nil (setq piki-font-lock-emphasis-regions
		     (cons (list (match-data) ,face)
			   piki-font-lock-emphasis-regions))))

(defun piki-font-lock-fontify-delayed-regions ()
  '((lambda (end)
      (if piki-font-lock-emphasis-regions
	  (let ((match (nth 0 (car piki-font-lock-emphasis-regions))))
	    (set-match-data match)
	    (goto-char (match-end 0))
	    (match-beginning 0))))
    (2 (prog1
	   (nth 1 (car piki-font-lock-emphasis-regions))
	 (setq piki-font-lock-emphasis-regions
	       (cdr piki-font-lock-emphasis-regions)))
       append)))

(defun piki-font-lock-matcher (regexp)
  `(lambda (end)
     (piki-font-lock-search-no-face ,regexp end t)))
(put 'piki-font-lock-matcher 'lisp-indent-function 1)

(defvar piki-mode-map nil)

;; TODO insert html tag region? ex: \<strong\><\/strong\>

(unless piki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l" 'piki-recenter-and-fontify)
    (setq piki-mode-map map)))

(defvar piki-font-lock-keywords nil
  "Default expressions to highlight in Piki modes.")

;;TODO
(setq piki-font-lock-keywords
      `((eval . (piki-font-lock-init))

	("^#.*" . font-lock-comment-face)

	;; <hr>
	("^=.*$" . piki-bold-face)

	;; <ul> and <ol>
	("^\\([-+]+\\)" (1 font-lock-function-name-face))

	;; <table>
	("^\\(|\\)\\(.*?\\)\\(|\\)$"
	 (1 piki-table-border-face)
	 (3 piki-table-border-face)
	 ("|"
	  (progn
	    (goto-char (match-beginning 2))
	    (match-end 2))
	  (goto-char (match-end 0))
	  (0 piki-table-rule-face)))

	;; <dt>
	("^\\(\\?.*\\)"
	 (1 font-lock-keyword-face))

	;; <dd>
	("^\\(\\!.*\\)"
	 (1 font-lock-keyword-face))

	;; (,(piki-font-lock-matcher	; <strong>
	;;       (concat "\\('''\\)\\([^'\n]?"
	;; 	      piki-any-char-regexp "*?\\)\\('''\\)"))
	;;  (1 font-lock-keyword-face)
	;;  ,(piki-fontify-later piki-bold-face)
	;;  (3 font-lock-keyword-face))
	;; (,(piki-font-lock-matcher	; <em>
	;;       (concat "\\(''\\)\\([^'\n]?" piki-any-char-regexp "*?\\)\\(''\\)"))
	;;  (1 font-lock-keyword-face)
	;;  ,(piki-fontify-later piki-italic-face)
	;;  (3 font-lock-keyword-face))

	;; [url]
	(,(piki-font-lock-matcher
	      "\\(\\[\\)\\(\\(?:\"[^\"]+\"\\)+\\|[^ \t\n]+\\)[ \t]\\([^]]+\\)\\(\\]\\)")
	 (1 font-lock-keyword-face)
	 (2 font-lock-reference-face)
	 (3 piki-link-face)
	 (4 font-lock-keyword-face))

	;; <img>
	(,(piki-font-lock-matcher
	      "^@ ?\\(\\(?:\"[^\"]+\"\\)+\\|[^ \t\n]+\\)[ \t]\\(.*\\)")
	 (1 font-lock-reference-face)
	 (2 piki-link-face))

	;; <h2>, <h3>, <h4>, ...
	("^\\(\\*+\\).*"
	 (0 (let* ((len (- (match-end 1) (match-beginning 1)))
		   (face-name (format "piki-header-%d-face" (min len 5))))
	      (intern face-name))
	    keep))

	;; fontify body portion of '''...''', ''...'', and :...:
	;; These fontifications are delayed until here.
	,(piki-font-lock-fontify-delayed-regions)

	;; ("^\\({\\)\\(?:\\([A-Z].*\\)\\|\\(.*\\)\\)"
	("^\\({\\)\\(.*\\)"
	 (1 font-lock-keyword-face)
	 (2 font-lock-function-name-face))
	("^\\(}\\)"
	 (1 font-lock-keyword-face))

	;; <pre>
	("^\\(>|\\)\n\\(\\(.*\n\\)*?\\)\\(|<\\)$"
	 (1 font-lock-keyword-face)
	 (2 piki-verbatim-face t)	; override faces
	 (4 font-lock-keyword-face))))

(define-derived-mode piki-mode text-mode "Piki"
  "Major mode to edit and commit Piki page.

\\{piki-mode-map}"
  (interactive)
  (set (make-local-variable 'font-lock-defaults)
       '(piki-font-lock-keywords t t nil backward-paragraph))
  (set (make-local-variable 'font-lock-multiline) t)
  (setq major-mode 'piki-mode)
  (setq mode-name "Piki")
  (set (make-local-variable 'comment-start) "#")
  (use-local-map piki-mode-map)
  (make-local-variable 'kill-buffer-hook)
  (run-hooks 'piki-mode-hook))

(defun piki-font-lock-search-no-face (regexp &optional bound noerror face)
  (or bound (setq bound (point-max)))
  (let ((saved (point))
	(move-to-beg-func (if face 'text-property-not-all 'text-property-any))
	(move-to-end-func (if face 'text-property-any 'text-property-not-all))
	found)
    (while (and (not found)
		(re-search-forward regexp bound t)) ; search roughly
      (let* ((beg (or (funcall move-to-beg-func (match-beginning 0) bound
			       'face face)
		      bound))
	     (end (or (funcall move-to-end-func beg bound 'face face) bound)))
	(if (and (< beg (match-beginning 0))
		 (< (match-end 0) end))
	    (setq found (point))
	  (goto-char beg)
	  (if (re-search-forward regexp end 'move)
	      (setq found (point))))))
    (or found
	(cond ((null noerror) (error "search failed"))
	      ((eq noerror t) (goto-char saved) nil)
	      (t (goto-char bound) nil)))))

(defun piki-recenter-and-fontify (&optional arg)
  "Center point in window, redisplay frame, and fontify the current buffer
if Font-Lock mode is enabled."
  (interactive "P")
  (recenter arg)
  (if font-lock-mode
      (font-lock-fontify-buffer)))

(provide 'piki-mode)

;;; piki-mode.el ends here
