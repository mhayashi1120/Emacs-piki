;;; piki-mode.el --- piki mode

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords:
;; Emacs: GNU Emacs
;; Package-Requires: ()

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; TODO about piki
;; copy from wiliki-mode.el
;; 
;; piki
;; * footnote <a href="#name" title=contents>
;;    like hatena?
;;    Text (((footnote contents))) ->
;;  -> Text<a href="#footnote-1" title="footnote contents">*1</a>

;; * strong, em 
;;  '''strong''' ''em''

;; * macro ${[A-Z]+} or $[A-Z]+
;;  like following.
;;   piki -MMACRO=VALUE template index.piki 

;; * line continuation like shell script.

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

(defun piki-font-lock--pre-matcher ()
  (lambda (limit)
    (let (start end
                start-tag-beg start-tag-end
                end-tag-beg end-tag-end)
      (cond
       ((not (re-search-forward "^>|$" limit t))
        nil)
       (t
        (setq start-tag-beg (match-beginning 0)
              start-tag-end (match-end 0))
        (setq start (point-at-bol))
        (cond
         ((re-search-forward "^|<$" nil t)
          (setq end (point-at-eol))
          (setq end-tag-beg (match-beginning 0)
                end-tag-end (match-end 0)))
         (t
          (with-silent-modifications
            (remove-text-properties (point) (point-max) '(fontified t)))
          (goto-char (point-max))
          (setq end (point-max))))
        (set-match-data
         (append
          (list start end
                start-tag-beg start-tag-end
                (1+ start-tag-end) (if end-tag-beg (1- end-tag-beg) end))
          (and end-tag-beg end-tag-end
               (list end-tag-beg end-tag-end))))
        t)))))

(defvar piki-mode-map nil)

(defun piki--re-title ()
  )

(defconst piki--re-title
  "\\(\\(?:\"\\(?:\\\\\"\\|[^\"]\\)+\"\\)\\|[^ \t\n]+\\)")

;; TODO insert html tag region? ex: \<strong\><\/strong\>

(unless piki-mode-map
  (let ((map (make-sparse-keymap)))

    (setq piki-mode-map map)))

(defvar piki-font-lock-keywords
  `(
    (,(piki-font-lock--pre-matcher)
     (1 font-lock-keyword-face)
     (2 'piki-verbatim-face)
     (3 font-lock-keyword-face nil t))

    ("^#.*" . font-lock-comment-face)

    ;; <hr>
    ("^=.*$"
     (0 'piki-bold-face))

    ;; <ul> and <ol>
    ("^\\([-+]+\\)"
     (1 font-lock-function-name-face))

    ;; <table>
    ("^\\(|\\)\\(.*?\\)\\(|\\)$"
     (1 'piki-table-border-face)
     (3 'piki-table-border-face)
     ("|"
      (progn
        (goto-char (match-beginning 2))
        (match-end 2))
      (goto-char (match-end 0))
      (0 'piki-table-rule-face)))

    ;; <dt>
    ("^\\(\\?.*\\)"
     (1 font-lock-keyword-face))

    ;; <dd>
    ("^\\(!.*\\)"
     (1 font-lock-keyword-face))

    ;; [url]
    (,(concat
       ;; line begining or not escaped "["
       "\\(?:^\\|[^\\\\]\\)"
       ;; beginning of bracket
       "\\(\\[\\)"
       ;; title string or double quoted string
       piki--re-title
       ;; separator of title and link
       "[ \t]+"
       ;; link
       "\\(\\(?:\\\\\]\\|[^\]]\\)+\\)"
       ;; end of bracket
       "\\(\\\]\\)"
       )
     (1 font-lock-keyword-face)
     (2 font-lock-reference-face)
     (3 'piki-link-face)
     (4 font-lock-keyword-face))

    ;; <img>
    (,(concat
       "^\\(@\\)"
       "[\s\t]+"
       "\\(?:"
       piki--re-title
       "[\s\t]+"
       ;; url
       "\\([^s\t\n]+\\)"
       "\\)?"
       )
     (1 font-lock-keyword-face)
     (2 font-lock-reference-face nil t)
     (3 'piki-link-face nil t))

    (,(concat
       "^\\(@@\\)"
       "[\s\t]+"
       "\\(?:"
       piki--re-title
       "[\s\t]+"
       ;; url
       "\\([^\s\t\n]+\\)"
       "[\s\t]+"
       ;; refurl
       "\\([^\s\t\n]+\\)"
       "\\)?"
       )
     (1 font-lock-keyword-face)
     (2 font-lock-reference-face nil t)
     (3 'piki-link-face nil t)
     (4 'piki-link-face nil t))
           
    ;; <h2>, <h3>, <h4>, ...
    ("^\\(\\*+\\).*"
     (0 (let* ((len (- (match-end 1) (match-beginning 1)))
               (face-name (format "piki-header-%d-face" (min len 5))))
          (intern face-name))))

    ;; <div>
    ("^\\({\\)\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("^\\(}\\)"
     (1 font-lock-keyword-face))

    )
  "Default expressions to highlight in Piki modes.")

;;;###autoload
(define-derived-mode piki-mode text-mode "Piki"
  "Major mode to edit and commit Piki page.

\\{piki-mode-map}"
  (interactive)
  (set (make-local-variable 'font-lock-defaults)
       '(piki-font-lock-keywords t t))
  (set (make-local-variable 'font-lock-multiline) t)
  (add-hook 'after-change-functions 'piki-after-change-buffer nil t)
  (setq major-mode 'piki-mode)
  (setq mode-name "Piki")
  (set (make-local-variable 'comment-start) "#")
  (use-local-map piki-mode-map)
  (make-local-variable 'kill-buffer-hook)
  (run-hooks 'piki-mode-hook))

(provide 'piki-mode)

;;; piki-mode.el ends here
