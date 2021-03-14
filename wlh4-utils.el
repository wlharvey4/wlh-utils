;;; wlh4-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;;; Author: wlh4
;;; Initial Commit: 2021-03-10
;;; Time-stamp: <2021-03-14 09:46:26 lolh>
;;; Version: 0.1.5

;;; Commentary:
;; Work with defs in a buffer.

;;; Code:

;; wlh4-defs: list of wlh4-defnm structs
;; wlh4-defnm: cl-struct: :name :desc :args :file :start :end :usages
;; :name symbol
;; :desc string
;; :args string NOTE: turn into a list of symbols
;; :file string
;; :start point or marker
;; :end point or marker
;; usages: list of point-or-marker's

(require 'cl-lib)
(defvar wlh4-defs "Property list of defs and defnm's")
(cl-defstruct wlh4-defnm name args desc file start end usages)
(defconst wlh4--dash "\n----------------------------------------------------------------------\n")

(defun wlh4-parse-defs (&optional buf)
  "List defined symbols in buffer `buf' or current buffer.

Print found information into a temporary buffer."
  (interactive)
  (setq wlh4-defs nil)
  (with-current-buffer
      (get-buffer-create (if buf buf (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (while
	  ;; Place list of defined symbols that should be found into const `d'
	  ;; TODO this finds 'defun in a macro; need to prevent that somehow
	  ;;(search-forward-regexp (regexp-opt list-of-defines 'symbols) nil t)
	  (search-forward-regexp "^(def" nil t)
	(let* ((def (symbol-at-point)) ; definition type as symbol
	       (st (line-beginning-position))
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
			   (buffer-substring-no-properties s e))
		       "")) ; return empty string if there are no arguments
	       (desc (progn ; 
		       (forward-line)
		       (if (looking-at "^[[:space:]]+[\"]")
			   (let ((b (car (match-data))))
			     (forward-sexp)
			     (concat "\n" (buffer-substring-no-properties b (point))))
			 "")))
	       (en (line-end-position)))
	  (setf wlh4-defs
		(plist-put wlh4-defs def
			   (cons
			    (make-wlh4-defnm :name nm
					     :args args
					     :desc desc
					     :file buffer-file-name
					     :start st
					     :end en)
			    (plist-get wlh4-defs def))))))))
  t)

;;; wlh4-utils.el ends here

(defun wlh4-defs (&optional buf)
  "Parse a file for defs, then print them sorted and categorized."
  (interactive)
  (with-output-to-temp-buffer "tempbuf"
    (wlh4-parse-defs buf)
    (print buffer-file-name)
    (setq defs wlh4-defs)
    (while defs
      (print (car defs))
      (prin1 (cadr defs))
      (setf defs (cddr defs)))))
