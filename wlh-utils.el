;;; wlh/utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;;; Commentary:

;;; Code:

(defconst file-default (expand-file-name "~/.config/emacs/elpa/org-plus-contrib-20210308/org-element.el"))
(defconst list-of-defines (list "defvar" "defun" "defsubst" "defconst" "defmacro defvaralias"))
(defconst dash "\n----------------------------------------------------------------------\n")

(defun listdefs (f)
  "List defined symbols in file `f'.

Print found information into a temporary buffer."
  (interactive "bBuffer to parse:")
  (with-current-buffer
      (get-buffer-create (find-file-noselect f))
    (goto-char (point-min))
    (with-output-to-temp-buffer "tempbuf"
      (while
	  ;; Place list of defined symbols that should be found into const `d'
	  ;; TODO this finds 'defun in a macro; need to prevent that somehow
	  (search-forward-regexp (regexp-opt list-of-defines 'symbols) nil t)
	(let* ((def (symbol-at-point)) ; definition type as symbol
	       (nm (progn (forward-char)(symbol-at-point))) ; name as symbol
	       (args (if ; add arguments if they exist
			 (or (eq def 'defun)
			     (eq def 'defsubst)
			     (eq def 'defmacro))
			 (let ((e (progn (forward-list)(point)))
			       (s (progn (backward-list)(point))))
			   (buffer-substring s e))
		       "")) ; return empty string if there are no arguments
	       (desc (progn ; 
		       (forward-line)
		       (if (looking-at "^[[:space:]]+[\"]")
			   (let ((b (car (match-data))))
			     (forward-sexp)
			     (concat "\n\n"
				     (buffer-substring-no-properties b (point))
				     dash))
			 "\n"))))
	  (princ (format "%s> %-10s: %-50s %s%s\n" dash def nm args desc)))))))

;;; wlh/utils.el ends here
