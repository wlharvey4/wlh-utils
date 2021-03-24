;;; wlh4-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;; Author: wlh4
;; Initial Commit: 2021-03-10
;; Time-stamp: <2021-03-24 01:38:09 lolh-mbp-16>
;; Version: 0.2.3



;;; Commentary:

;;  wlh4-parse-defs:
;;  ----------------
;;
;;  Parse a buffer for its various definition commands; place all data
;;  into a global variable `wlh4-defs'.
;;
;;  wlh4-defs:
;;  ----------
;;
;;  Print   all  defined   symbols   derived  from   `wlh4-parse-defs'
;;  alphabetically  in a  buffer, with  parameters, descriptions,  and
;;  positions in file.
;;
;; TODO:
;;   - Calculate usages
;;   - Print only first line of description;
;;     rest of description is invisible but clickable
;;     to reveal.
;;
;; wlh4-preorder-traversal:
;; ------------------------
;; Procedure to walk an org tree using the preorder traversal method.



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

(defvar wlh4-defs nil
  "Global property list of definition commands and defined symbol names and properties.")
(cl-defstruct wlh4-defnm
  "Structure to hold defined names, argument lists, descriptions, etc."
  name args desc desc-st desc-en file start end usages)
(defconst wlh4--dash
  "\n---------------------------------------------------------------------"
  "Separator line when a define has a description.")

(defun wlh4-parse-defs (&optional buf)
  "Parse buffer `buf' (default current buffer) for all defines.

Store  all data  in  a global  plist  of defines  and  a list  of
structures  containing information.   Store the  information into
the  global   variable  wlh4-defs,  which  other   functions  can
reference and use."
  (interactive)
  (setq wlh4-defs nil)
  (with-current-buffer
      (get-buffer-create (if buf buf (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (while
	  ;; find all `defines' in the buffer
	  (search-forward-regexp "^(\\(?:cl-\\)?def" nil t)
	(let* (desc-st
	       desc-en
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




;; wlh4-preorder-traversal
;; -----------------------
;; Walk an Org Tree using the Preorder Traversal method
;; See https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/GenTreeIntro.html

(defun wlh4-init-org-tree (buf)
  "Obtain an OrgTree data structure for a buffer."
  (with-current-buffer buf
    (org-element-parse-buffer)))

(defun _prop-keys (props)
  "Create a string of key symbols from a plist."
  (let ((prop-str ""))
    (while props
      (let ((key (symbol-name (pop props))))
	(setf prop-str (format "%s%s" prop-str key))
	(pop props)))
    prop-str))

;; Preporder Traversal of a General Tree
;;  https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/GenTreeIntro.html
(defun wlh4-org-tree-traversal (org-node level)
  "Performs a preorder traversal of an OrgTree, a root OrgNode.

An  OrgTree  node  (`OrgNode')  is a  list  containing  a  `type'
designator, a  plist of properties  relevant to the type,  and an
indefinite number of child OrgNodes.

  OrgNode: (<type> (plist ...) (child OrgNode) (child OrgNode) ...)

The key to traversing an OrgTree is knowing that the Org function
`org-element-contents' returns a list of  child nodes if they are
present, which list  can be traversed recursively.   If there are
no child  nodes, then this function  does not return a  list, and
no further recursion takes place.

Also, sometimes the OrgNode is not  a readable list, and so it is
not possible to return a list of properties, even though there is
a type.

This function  simply prints some relevant  information about the
current OrgNode.  It  keeps track of the level,  prints the level
number, and indents  the information by `level'  spaces.  It also
prints  a  :raw-value  or  :value   if  one  is  present  in  the
properties.   Finally,  it  prints  a string  of  keys  from  the
plist (without values) for reference purposes."
  (let* ((type (org-element-type org-node))
	 (props (and (listp org-node) (second org-node)))
	 (contents (org-element-contents org-node))
	 (raw-val (plist-get props :raw-value))
	 (val (plist-get props :value)))
    (princ
     (format "%2d]%s%s[%s]: %s \n"
	     level
	     (make-string level 32)
	     type
	     (if raw-val
		 (format "\"%s\""raw-val)
	       (if val
		   (format "\"%s\"" val) ""))
	         (_prop-keys props)))
    (if (listp  contents); Only  continue and recurse if  org-node is
			 ; not a leaf node (i.e. has no child nodes.)
	(let ((child (first contents))
	      (children (rest contents)))
	  (while child
	    (wlh4-org-tree-traversal child (1+ level))
	    (setf child (first children))
	    (setf children (rest children))))))
  t)

(defun wlh4-walk-org-tree (&optional buf)
  "Walk an OrgTree using the Org buffer `buf'.

By default use the current buffer."
  (interactive "bBuffer to parse: ")
  (let ((org-buf (if buf buf (current-buffer))))
    (with-temp-buffer-window "*OrgTree*" nil nil
	(wlh4-org-tree-traversal
	 (wlh4-init-org-tree org-buf) 0))))

;; USAGE: (wlh4-walk-org-tree "walk.org")
;;        M-x wlh4-walk-org-tree

;;; wlh4-utils.el ends here
