;;; wlh-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;;; Author: l-o-l-h
;;; Initial Commit: 2021-03-10
;;; Time-stamp: <2021-03-11 19:27:02 lolh-mbp-16>
;;; Version: 0.1

;;; Commentary:

;;; Code:

;; (def... ((defname (:desc ... :args ... :file ... :start ... :end ... (point point point))) (defname ...) ...) def... (()))
;; (add-defname def defname desc args file start end)
;; (add-defname def defname point)
(defvar defs)
(defconst dash "\n----------------------------------------------------------------------\n")

(defun listdefs (&optional buf)
  "List defined symbols in buffer `buf' or current buffer.

Print found information into a temporary buffer."
  (interactive)
  (with-current-buffer
      (get-buffer-create (if buf buf (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (with-output-to-temp-buffer "tempbuf"
	(while
	    ;; Place list of defined symbols that should be found into const `d'
	    ;; TODO this finds 'defun in a macro; need to prevent that somehow
	    ;;(search-forward-regexp (regexp-opt list-of-defines 'symbols) nil t)
	    (search-forward-regexp "^(def" nil t)
	  (let* ((def (symbol-at-point)) ; definition type as symbol
		 (nm (progn
		       (skip-chars-forward "^[[:space:]]")
		       (forward-char 2)
		       (symbol-at-point)))
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
	    (princ (format "%s> %-10s: %-50s %s%s\n" dash def nm args desc))))))))

(defun add-defname (def defname desc args file start end)
  )

;;; wlh-utils.el ends here
