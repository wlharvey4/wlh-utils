;;; wlh4-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;;; Author: wlh4
;;; Initial Commit: 2021-03-10
;;; Time-stamp: <2021-03-16 23:25:26 lolh-mbp-16>
;;; Version: 0.1.9

;;; Commentary:

;;  Print  defs  alphabetically  in  a  buffer,  with  parameters  and
;;  position in file.

;;; TODO:
;;   - Calculate usages
;;   - Print only first line of description;
;;     rest of description is invisible but clickable
;;     to reveal.

;;; Code:

;; wlh4-defs: plist of wlh4-defnm structs, sorted alphabetically
;; wlh4-defnm: cl-struct: :name :desc :desc-st :desc-en :args :file :start :end :usages
;; :name symbol
;; :desc string
;; :desc-st point
;; :desc-en point
;; :args string TODO: turn into a list of symbols
;; :file string
;; :start point of def
;; :end point of def
;; :usages: list of point-or-marker's

(require 'cl-lib)
(require 'seq)

(defvar wlh4-defs "Property list of defs and defnm's")
(cl-defstruct wlh4-defnm name args desc desc-st desc-en file start end usages)
(defconst wlh4--dash "\n---------------------------------------------------------------------")
(setq filter-buffer-substring-function
      (lambda (b e &optional d)
	(let ((s (buffer-substring b e)))
	  (replace-regexp-in-string "\n" "" s))))

(defun wlh4-parse-defs (&optional buf)
  "Parse buffer `buf' (default current buffer) for all defines.

Store all  data in a  plist of defines  and a list  of structures
containing information.   Store the  information into  the global
variable wlh4-defs, which other functions can reference and use."
  (interactive)
  (setq wlh4-defs nil)
  (with-current-buffer
      (get-buffer-create (if buf buf (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      ;; (setq filter-buffer-substring-function
	    ;; (lambda (s e d)
	      ;; (while (re-search-forward "\n" en t)
		;; (replace-match ""))))
      (while
	  ;; find all `defines' in the buffer
	  (search-forward-regexp "^(def" nil t)
	(let* ((desc-st) (desc-en)
               (def (symbol-at-point)) ; definition type as symbol
	       (st (line-beginning-position))
	       ;; The cursor is in the first symbol, the def name.
	       ;; The following excursion moves to the beginning
	       ;; of the function, then jumps to the end to find the
	       ;; the end position, then returns to where it started.
               (en (save-excursion
                     (backward-up-list)
                     (forward-list)
                     (point)))
	       (nm (progn ; find the def name
		     ;; search forward to a space and skip possible quote char
		     (search-forward-regexp "\\([[:space:]]+\\(?:[']?\\)\\)")
		     (symbol-at-point))) ; grab the symbol name at point
	       ;; many things have args, but not all, and some are optional
	       (args (cond ((or (eq def 'defun) ; these have args enclosed in parens
				(eq def 'defsubst)
				(eq def 'defmacro))
			    (let ((e (progn (forward-list)(point)))
				  (s (progn (backward-list)(point))))
			      (buffer-substring-no-properties s e)))
			   ((or (eq def 'defvar)
				(eq def 'defconst))
			    (forward-symbol 1)
			    (skip-syntax-forward "-")  ; skip whitespace
			    (if (looking-at-p "[)\n]") ; then no default value
				"<>"
			      (concat "<"
				       (buffer-substring-no-properties
					(point)
					(progn (forward-sexp)(point)))
				      ">")))
			   (t "<>")))
	       (desc (progn
		       (forward-line)
		       (if (looking-at "^[[:space:]]+[\"]")
			   (let ((b (car (match-data)))
                                 (desc-st (line-beginning-position)))
			     (forward-sexp)
                             (setq desc-en (point))
			     (buffer-substring-no-properties b (point)))
			 ""))))
	       ;; TODO: Find all usages at this point
	  (setf wlh4-defs
		(plist-put wlh4-defs def
			   (cons
			    (make-wlh4-defnm :name nm
					     :args args
					     :desc desc
                                             :desc-st desc-st
                                             :desc-en desc-en
					     :file buffer-file-name
					     :start st
					     :end en)
			    (plist-get wlh4-defs def))))))))
  t)

;;; wlh4-utils.el ends here

(defun wlh4-defs (&optional buf)
  "Parse a file for defs, then print them sorted and categorized."

  (interactive) ; TODO: allow for arbitrary buffer
  (with-output-to-temp-buffer "tempbuf"
    (wlh4-parse-defs buf)
    (print buffer-file-name)
    (setq defs wlh4-defs)
    (while defs
      (let ((def (car defs))
	    (defnms (seq-sort
		     (lambda (a b) (string-greaterp (wlh4-defnm-name b)
						    (wlh4-defnm-name a)))
		     (cadr defs))))
	(print def)
	(dolist (defnm defnms)
	  ;; TODO: create hidden folded descriptions
	  ;; that unfold at a touch.
	  ;; TODO: create hideen lists of usages that unfold
	  (princ (format ">  %s %s [%d--%d]%s\n"
			 (wlh4-defnm-name defnm)
			 (wlh4-defnm-args defnm)
			 (wlh4-defnm-start defnm)
			 (wlh4-defnm-end defnm)
                         (let ((desc (wlh4-defnm-desc defnm)))
                           (if (not (string-empty-p desc))
                               (concat "\n" desc wlh4--dash)
                             ""))))))
      (setf defs (cddr defs)))))
