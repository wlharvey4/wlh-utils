;;; wlh4-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;; Author: wlh4
;; Initial Commit: 2021-03-10
;; Time-stamp: <2021-04-22 03:08:50 lolh-mbp-16>
;; Version: 0.6.6



;;; Commentary:

;;  wlh4-parse-defs:
;;  ----------------
;;  Parse a buffer for its various definition commands; place all data
;;  into a global variable `wlh4-defs'.
;;
;;
;; wlh4-defnm: cl-struct containing:
;; -----------
;; :name symbol
;; :desc string
;; :desc-st point
;; :desc-en point
;; :args string TODO: turn into a list of symbols
;; :file string
;; :start point of def
;; :end point of def
;; :usages: list of point-or-marker's
;;
;;
;;  wlh4-defs:
;;  ----------
;;  plist of wlh4-defnm structs, sorted alphabetically
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
;;
;;
;; wlh4-walk-org-tree:
;; -------------------
;; Wrapper for wlh4-org-tree-traversal
;;
;; wlh4-org-tree-traversal:
;; ------------------------
;; Procedure to walk an org tree using the preorder traversal method.
;;
;;
;;
;; wlh4-find-clock-entries:
;; ------------------------
;; Wrapper for wlh4-traverse-clock-entries
;;
;; wlh4-traverse-clock-entries:
;; -------------------
;; Procedure to walk an org tree using the preorder traversal method
;; and extract and parse all clock entries.  Report errors for certain
;; problems, like a running clock, a zero duration, an entry without
;; a case tag.



;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)

;;; wlh4-parse-defs:
;;; ----------------

(defvar wlh4-defs nil
  "Global property  list of definition commands.

Includes defined symbol names and properties.")

(cl-defstruct wlh4-defnm
  "Structure to hold defined names, argument lists, descriptions, etc."
  name args desc desc-st desc-en file start end usages)
(defconst wlh4--dash
  "\n---------------------------------------------------------------------"
  "Separator line when a define has a description.")

;; USAGE: wlh4-defs <RET> [elisp-buffer]
;;        (wlh4-def [elisp-buffer])
(defun wlh4-defs (&optional buf)
  "Parse a file (defaulting to BUF) for defined functions.

Then print them sorted and categorized."

  (interactive) ; TODO: allow for arbitrary buffer
  (with-output-to-temp-buffer "tempbuf"
    (wlh4-parse-defs buf)
    (print buffer-file-name)
    (let ((defs wlh4-defs))
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
	(setf defs (cddr defs))))))

(defun wlh4-parse-defs (&optional buf)
  "Parse buffer `BUF' (default current buffer) for all defines.

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


;;; wlh4-org-tree-traversal:
;;; ------------------------

;; Walk an Org Tree using the Preorder Traversal method
;; See https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/GenTreeIntro.html

;; COMMAND: wlh4-walk-org-tree ORG-BUF
;; USAGE:   M-x wlh4-walk-org-tree <RET> buffer
;;          (wlh4-walk-org-tree "walk.org")

(defun wlh4-walk-org-tree (org-buf)
  "Command to walk (traverse) an OrgTree of an Org buffer `ORG-BUF'."

  (interactive "bBuffer to parse: ")
  (with-temp-buffer-window "*OrgTree*" nil nil
    (wlh4-org-tree-traversal (wlh4-parse-org-buffer org-buf) 0)))



;; Some utility functions
(defun wlh4-parse-org-buffer (buf)
  "Utility to parse an Org-mode buffer BUF into an OrgTree."
  (with-current-buffer buf
    (org-element-parse-buffer)))

(defun pc--props (props indent)
  "Given a plist of PROPS, print them as a string.

INDENT is the indentation based upon the level."
  (princ
   (format "   %s%s\n"
	   indent
	   (mapconcat
	    (lambda (p) (cond ((symbolp p) (symbol-name p))
			      (t (format "%s" p))))
	    props " "))))

(defconst +common-keys+
  '(:begin :post-affiliated :end :post-blank :contents-begin :contents-end :parent))
(defconst +aff-keys+ ; affiliated keywords
  '(:caption :header :name :plot :results :attr_))
(defconst +element-keys+
  '("clock" '(:duration :status :value)
    "drawer" '(:drawer-name)
    "headline" '(:archivedp :closed :commentedp :deadline :footnote-section-p :level :pre-blank :priority :quotep :raw-value :scheduled :tags :title :todo-keyword :todo-type)
    "item" '(:bullet :checkbox :counter :pre-blank :raw-tag :tag :structure)
    "keyword" '(:key :value)
    "plain-list" '(:structure :type)
    "planning" '(:closed :deadline :scheduled)
    "timestamp" '(:day-end :day-start :hour-end :hour-start :minute-end :minute-start :month-end :month-start :raw-value :repeater-type :repeater-value :type :warning-type :warning-unit :warning-value :year-end :year-start)
    ))



;; Main routine: Preporder Traversal of a General Tree
(defun wlh4-org-tree-traversal (org-node level)
  "Perform a `preorder' traversal of an OrgTree.

This  function  traverses  an  OrgTree  starting  from  the  root
node   (ORG-NODE)   obtained   from  the   org-element   function
`org-element-parse-buffer'.   This functions  keeps track  of the
current LEVEL.  An  OrgTree node (`OrgNode') is  a recursive list
data structure containing either:

- type: a `type' designator identifying the type of node
- props: a plist of properties relevant to the type, and
- contents: an indefinite number of child OrgNodes,

or

- a secondary string representing a plain-text element;

Thus, an OrgNode is one of:

-  OrgNode: (<type> (plist ...) (child OrgNode) (child OrgNode) ...)
or
-  OrgNode: #(\"string\" # # plist ...)

The key to traversing an OrgTree  is knowing that the OrgNode can
be  either a  list like  `(type props  children)' or  a secondary
string.   If  it  is  a  list,  then  the  Org  Element  function
`org-element-contents' called with an  OrgNode returns a possibly
empty  list  of  child  OrgNodes, which  list  can  be  traversed
recursively  using the  preorder  tree  traversal algorithm.   If
there are  no child nodes,  or if  the OrgNode is  a `plain-text'
secondary string type, then this function does not return a list,
and no further  recursion takes place.  You must  be careful when
parsing into an OrgNode because some list functions will throw an
error when run on a secondary string.

This  procedure  walks  an   OrgTree  and  prints  some  relevant
information about  the current  OrgNode.  It  keeps track  of the
current level, prints  the current level number,  and indents the
information  by  `level'  dots  and spaces.   It  also  prints  a
`:raw-value' or `:value' or `plain-text'  string value, if one of
those is  present in the properties  (for the values) or  is that
type (for the plain-text).  It also  prints a string of keys from
the plist (without values) for reference purposes."

  ;; 1. parse the current OrgNode into:
  ;;    - type: one of `org-data|element|object|plain-text|nil'
  ;;    - class: one of `element|object'
  ;;    - props: plist of properties or plain-text secondary string
  ;;    - contents: child OrgNode, list of child OrgNodes, or nil
  ;;    - `value' or `KEY: value' when present

  ;; use org-element functions for parsing when available
  (let* ((type     (org-element-type org-node))
	 (class    (org-element-class org-node))
	 (contents (org-element-contents org-node)) ; nil|element|list elements

	 ;; must check for a list (cons cell) in the properties
	 ;; if not a list, then the entire OrgNode is a plain-text secondary string
	 (props
	  (if (consp org-node)  ; OrgNode can have a plist or be a secondary string
	      (second org-node) ; here, OrgNode has a plist in 2nd position
	    org-node))          ; here, OrgNode is a secondary string

	 (c-props ; common properties to all elements and objects
	  (when props
	    (seq-mapcat
	     (lambda (ck)
	       (let ((v (plist-get props ck)))
		 (when (eq ck :parent) ; include only type of parent node
		   (setf v (org-element-type v)))
		 (when v
		   (list ck v))))
	     +common-keys+)))

	 (t-props ; type-specific properties
	  (when (consp props)
	    (let* ((all-props (copy-sequence props))
		   (rest nil)
		   (rest-props (progn (while all-props
				 (let ((cp (pop all-props))
				       (cv (pop all-props)))
				   (unless (memq cp +common-keys+)
				     (setf rest (cons cp (cons cv rest))))))
				      rest)))
	      (when (and (string= type "headline") ; traverse a headline's title properties
			 (plist-get rest-props :title))
		(let* ((title (plist-get rest-props :title))
		       (child (car title))
		       (children (cdr title))
		       (ti ()))
		  (while child
		    (let* ((ty (org-element-type child))
			   (ty1
			    (cond ((string= ty "link") ; handle links specially
				   (plist-put (second child) :parent
					      (org-element-type
					       (plist-get (second child) :parent))))
				  ((string= ty "plain-text") (format "\"%s\"" child))
				  (t ty))))
		      (setf ti (cons (cons ty ty1) ti)))
		    (setf child (car children))
		    (setf children (cdr children)))

		  (plist-put rest-props :title (reverse ti))))
	      rest-props)))

	 ;; show the level of recursion through indentation
	 (level-indent (make-string level 32))    ; spaces
	 (level-indent-dot (make-string level ?.))) ; dots

    ;; 2. print the current OrgNode information
    (princ
     (format "%2d]%s%s (%s)%s\n"
	     level
	     level-indent-dot
	     type
	     class
	     (if (stringp props) (concat " \"" (string-trim props) "\"") "")))

    (when t-props ; print the type-properties
      (pc--props t-props level-indent))
    (when (consp c-props) ; print the common properties
      (pc--props c-props level-indent))
    (terpri)

    ;; 3. recurse into contents, i.e., child OrgNodes, if such exist
    (if (listp  contents) ; don't try to recurse into a secondary string
	(let ((child (first contents))
	      (children (rest contents)))
	  (while child
	    (wlh4-org-tree-traversal child (1+ level)) ; recurse
	    (setf child (first children))
	    (setf children (rest children))))))

  ;; 4. all done; return true
  t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wlh4-find-clock-entries
;; Use the procedure  of traversing an Org Tree  but work specifically
;; on Clock entries only.


;;;--------------------------------------------------------------------->
(cl-defstruct wlh4-worklog-entry
  "Structure to hold a worklog entry."

  headlines 	; (list of strings)
  detail 	; (string)
  c-props 	; (plist of common properties)
  t-props) 	; (plist of clock properites)
;;;--------------------------------------------------------------------->


;;;--------------------------------------------------------------------->
(defvar *wlh4-all-worklog-entries*
  "List by line position of wlh4-worklog-entry elements.")

(defvar *wlh4-all-worklog-entries-sorted*
  "Sorted list (by either  time or case-time) of wlh4-worklog-entry
elements.")

(defvar *wlh4--overlap-windows* nil
  "Holds 4 variables:  

- `org-buf',
- `indirect-org-buf',
- `org-window',
- `indirect-org-window'.")
;;;--------------------------------------------------------------------->


;;;--------------------------------------------------------------------->
(defvar *wlh4--by-switch* 'time
  "Switch used to determine whether to sort :by 'time or :by 'case.")

(defun wlh4--set-by-switch (arg)
  "Set the variable `*wlh4--by-switch*' to 'time or 'case.

ARG receives the value of a prefix argument (4) or none (1).
With no prefix argument, set to 'time; with a prefix argument set
to 'case."

  (interactive "p")
  (setq *wlh4--by-switch*
	(cond ((= arg 1) 'time)
	      ((= arg 4) 'case)
	      (t (user-error "Incorrect prefix argument"))))
  (message "sort :by %s" *wlh4--by-switch*))

(define-key org-mode-map (kbd "C-c b") 'wlh4--set-by-switch)
;;;--------------------------------------------------------------------->


;;;--------------------------------------------------------------------->
(defconst +wlh4--line+ (format " %s" (make-string 78 ?-))

  "String of dashes to surround the detail of a printed wl-entry.")

(defconst +ts-re--inactive+
  "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +[^]+0-9>\r\n -]+ +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)\\]"

  "A regular expression representing an inactive timestamp.
This  is taken  from `org-ts-regexp0'  in `org.el'  but with  all
subexpressions eliminated and all optional parts made required.")

(defconst +tsr-re--inactive+
  (concat +ts-re--inactive+ "--" +ts-re--inactive+)

  "A regular expression representing an inactive timestamp range.")

(defconst +date-re+
  "\\([[:digit:]]\\{4\\}\\)\\(?:-\\([[:digit:]]\\{2\\}\\)\\)?\\(?:-\\([[:digit:]]\\{2\\}\\)\\)?"

  "Date regexp with required year, optional month and optional day.")

(defconst +hour:min-re+
  "\\([[:digit:]]\\{1,2\\}\\):\\([[:digit:]]\\{2\\}\\)"

  "Hours:Mins duration regexp.")
;;;--------------------------------------------------------------------->


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TODO: option to run on full org-buf or only visible portion
;;        right now it runs only on visible portion
(defun wlh4-find-clock-entries (&optional org-buf display)
  "Wrapper for main routine; user will select ORG-BUF.

With non-nil optional DISPLAY, do  display results of search to a
temporary buffer."
  (interactive)
  (unless org-buf (setq org-buf (current-buffer)))
  (setf *wlh4-all-worklog-entries* nil) ; start fresh
  (message "%s"
   (catch 'clock-problem
     (with-current-buffer org-buf
       (if display
	   (with-temp-buffer-window "*OrgClocks*" nil nil
	     (wlh4-traverse-clock-entries (wlh4-parse-org-buffer org-buf) 0 'display))
	 (wlh4-traverse-clock-entries (wlh4-parse-org-buffer org-buf) 0))))))


;;;--------------------------------------------------------------------->
(defun wlh4-find-clock-entries-sorted (&optional org-buf)
  "Wrapper for main routine to use ORG-BUF and sort the list.

This function  consults the global variable  `*wlh4--by-switch*' to
determine whether  to sort `:by  'time' or `:by case'.   Set this
variable  first using  the function  `wlh4--set-by-switch'.  This
routine stores the  sorted entries, and does  not return anything
nor print anything to temporary buffer."

  (interactive)
  (unless org-buf (setq org-buf (current-buffer)))
  (wlh4-find-clock-entries org-buf)
  (setq *wlh4-all-worklog-entries-sorted*
	(wlh4-sort-all-worklog-entries :by *wlh4--by-switch*)))

(define-key org-mode-map
  (kbd "C-c s") 'wlh4-find-clock-entries-sorted)
;;;--------------------------------------------------------------------->
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun wlh4--setup-overlap-windows (org-buf &optional reset)
  "Make sure the variable `*wlh4--overlap-windows*' has content.

Use ORG-BUF if necessary.  If RESET is non-nil, reset all
variables."
  (when reset
    (when (bufferp (second *wlh4--overlap-windows*))
      (kill-buffer (second *wlh4--overlap-windows*)))
    (when (and
	   (window-valid-p (fourth *wlh4--overlap-windows*))
	   (not (eq (fourth *wlh4--overlap-windows*) (selected-window))))
      (delete-window (fourth *wlh4--overlap-windows*)))
    (setq *wlh4--overlap-windows* nil))
  (unless *wlh4--overlap-windows*
    (outline-show-all)
    (let* ((ob (get-buffer org-buf))
	   (ib (make-indirect-buffer ob "ib" t))
	   (wob (get-buffer-window ob))
	   (wib (or (get-buffer-window ib)
		    (progn
		      (let ((wib (split-window wob nil 'right)))
			(set-window-buffer wib ib t)
			wib)))))
      (setq *wlh4--overlap-windows* (list ob ib wob wib)))))


;;;--------------------------------------------------------------------->
(defun wlh4-find-clock-entries-sorted-overlaps-removed (&optional org-buf reset)
  "Sort the list and report any overlaps.

When run from elisp, include  ORG-BUF, otherwise, run the command
from the ORG-BUF buffer and it will default to using that buffer.
When   RESET  is   non-nil   (single   prefix  argument),   reset
`*wlh4--overlap-windows*' variable.   The wl entries must  not have been
sorted by 'case when checking for overlaps."

  (interactive "i\np")
  (when (eql *wlh4--by-switch* 'case)
    (user-error "Must not have sorted entries by 'case when checking for overlaps"))
  (unless org-buf (setq org-buf (current-buffer)))
  (wlh4--setup-overlap-windows org-buf (when (or (= reset 4) reset) t))
  (message "%s"
	   (catch 'end-verify
	     (let ((prior-wl-entry nil))
	       (dolist (current-wl-entry *wlh4-all-worklog-entries-sorted*)
		 (when prior-wl-entry ; skip the first entry
		   (let ((ct1 (ts--begin current-wl-entry))
			 (ct2 (ts--end current-wl-entry))
			 (pt1 (ts--begin prior-wl-entry))
			 (pt2 (ts--end prior-wl-entry))
			 (ob  (first *wlh4--overlap-windows*))
			 (ib  (second *wlh4--overlap-windows*))
			 (wob (third *wlh4--overlap-windows*))
			 (wib (fourth *wlh4--overlap-windows*)))

		     (when (org-time< ct1 pt2)
		       ;; there exist overlapping clocks; show them side-by-side
		       (set-window-point wib (+ (ts--l prior-wl-entry) 19))
		       (set-window-point wob (+ (ts--l current-wl-entry) 19))
		       (throw 'end-verify "Overlapping times"))))
		 ;; keep track of the prior entry and continue loop
		 (setq prior-wl-entry (copy-wlh4-worklog-entry current-wl-entry))))
	     ;; When reaching here, all overlaps have been removed; clean up
	     (kill-buffer (second *wlh4--overlap-windows*))
	     (delete-window (fourth *wlh4--overlap-windows*))
	     (goto-char (point-min))
	     (org-set-startup-visibility)
	     (throw 'end-verify "Successfully completed verify"))))

(define-key org-mode-map
  (kbd "C-c o") 'wlh4-find-clock-entries-sorted-overlaps-removed)
;;;--------------------------------------------------------------------->


(defun wlh4--extract-common-keys (all-props)
  "Utility function to separate ALL-PROPS into common props and type props.

It also separates out the parent and replaces its value with the
type of the parent to make any printed output easier to read.
This function returns a list of three elements: the common
properties, the type properties and the parent."
  (let (t-props c-props (parent (plist-get all-props :parent)))
    (while all-props
      (let* ((k (car all-props))
	     (v (if (eq k :parent)
		    (org-element-type (cadr all-props))
		  (cadr all-props))))
	(if (memq k +common-keys+)
	    (setf c-props (cons v (cons k c-props)))
	  (setf t-props (cons v (cons k t-props))))
	(setf all-props (cddr all-props))))
    (list (reverse c-props) (reverse t-props) parent)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wlh4-traverse-clock-entries
;; Main routine to parse clock elements for worklog information.
(defun wlh4-traverse-clock-entries (org-node level &optional display)
  "Recursively extract all clock ORG-NODE's from an org buffer.

Keep track of  the current LEVEL during  recursion.  With non-nil
optional  DISPLAY,  do  print  the  results  of  traversal  to  a
temporary buffer."

  ;; I. Extract the ORG-NODE contents
  (let* ((type (org-element-type org-node))
	 (contents (org-element-contents org-node))
	 (props (if (consp org-node)
		    (second org-node)
		  org-node)))

    ;; II. Process only clock entries and extract contents
    (when (string= type "clock")
      (cl-multiple-value-bind (c-props t-props parent)
	  ;; returns common properties, clock type properties, clock's
	  ;; parent element
	  (wlh4--extract-common-keys props)

	;; find timestamp element, duration, status, raw-value, tags, headings
	(let* ((tse (plist-get t-props   :value))     ; clock's timestamp element
	       (dur (plist-get t-props   :duration)) ; clock's duration
	       (stat (plist-get t-props  :status))  ; clock's status
	       (rv (org-element-property :raw-value tse)) ; tse's raw value
	       (tags (org-get-tags (plist-get c-props :begin))) ; list of all tags

	       (tag ; case tag only
		(seq-some
		 (lambda (tag)
		   (when (string-match-p "[[:digit:]]\\{6\\}" tag)
		     (substring-no-properties tag)))
		 tags))

	       (headlines ; all foregoing headlines
		(let (hl (datum parent))
		  (catch 'headline1
		    (while t
		      (when (string= (org-element-type datum) "headline")
			(let ((h-rv (format "%s" (org-element-property :raw-value datum))))
			  (setf hl (cons h-rv hl)))
			(when
			    (eql (org-element-property :level datum) 1)
			  (throw 'headline1 hl)))
		      (setf datum (org-element-property :parent datum))))))

	       (detail ; description of what happened during the
		       ; timestamp period
		;; Find the plain-list item after the clock element;
		;; find each item in the plain-list; obtain the
		;; plain-text contents of each item, and catenate all
		;; together. Remove all extraneous whitespace
		;; (including newlines)

		;; TODO: Plain-text items can have objects (such as timestamps)
		;;       inside of them; need to make sure those are parsed and
		;;       included with the message.
		;; TODO: Most clocks have only one plain list with one list time,
		;;       but this code should be able to concatenate more than one.

		(when
		    (string= (org-element-type (first children)) "plain-list")
		  (let* ((pl (first children)) ; the next element should be a plain-list
			 (lis (org-element-contents pl)) ; it should contain a list of items
			 (txt "")) ; variable to hold the details
		      (string-clean-whitespace
		       (dolist (li lis txt) ; run through all items; usually one one, but...
			 (let* ((par (first (org-element-contents li)))
				(plain (substring-no-properties (first (org-element-contents par)))))
			   (setf txt (concat txt " " plain)))))))))

       	  (when display
	    (princ
	     (format "%s: {%s} %s %s (%s)\n%s\n%s\n  %s\n  %s\n\n"
		     type tag rv dur stat headlines detail c-props t-props)))

	  ;; Find and report some serious clocking problems.
	  ;; Will open a second window into the buffer and
	  ;; place the cursor at the problem clock with a
	  ;; message.
	  (let* ((ts-ok (string-match +tsr-re--inactive+ rv))
		 (dur-hr (progn (string-match "\\(^[[:digit:]]\\{1,2\\}\\):" dur)
				(string-to-number (match-string 1 dur))))
		 (clock-problem
		  (cond
		   ((not ts-ok) "Malformed timestamp")
		   ((string= stat "running") "Running clock")
		   ((string= dur "0:00") "Zero duration")
		   ((> dur-hr 8) "Extended duration")
		   ((null tag) "Missing tag")
		   ((or (null detail) (string-empty-p detail)) "Missing detail")
		   (t nil))))
	    (when clock-problem
	      (print clock-problem)
	      (message "%s: %s" clock-problem t-props)
	      (switch-to-buffer (current-buffer))
	      (goto-char (plist-get c-props :begin))
	      (org-reveal)
	      (throw 'clock-problem clock-problem))

	    ;; The timestamp seems to be in good form;
	    ;; Round the wl-entry as necessary
	    (let ((min-s (mod (org-element-property :minute-start tse) 6))
		  (min-e (mod (org-element-property :minute-end   tse) 6)))
	      (when
		  (or (cl-plusp min-s) (cl-plusp min-e))
		;; place point at the beginning of the first timestamp
		(goto-char (1+ (org-element-property :begin tse)))
		(unless (zerop min-s)
		  (org-timestamp-change (- min-s) 'minute))
		(unless (zerop min-e)
		  ;; place point at the beginning of the second timestamp
		  (search-forward "]--[")
		  (org-timestamp-change (- 6 min-e) 'minute)))))

	  ;; III. Store the clock before continuing traversal
	  ;;      All necessary information is in these four items
	  (push (make-wlh4-worklog-entry
		 :headlines headlines ; the first list item is the case
		 :detail detail       ; string telling what was done
		 :t-props t-props     ; contains the timestamp and duration
		 :c-props c-props)    ; can be used to locate the clock
		*wlh4-all-worklog-entries*))))

    ;; IV. Traverse the current Org-node's children
    (when (listp contents)
      (let ((child (first contents))
	    (children (rest contents)))
	(while child
	  (wlh4-traverse-clock-entries child (1+ level) display)
	  (setf child (first children))
	  (setf children (rest children))))))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; ACCESSOR FUNCTIONS
;;;============================================================================
(defun wlh4-timestamp-from-worklog-entry (wl-entry)
  "Return the full timestamp from a WL-ENTRY."

  (plist-get (wlh4-worklog-entry-t-props wl-entry) :value))

(defalias 'ts--e #'wlh4-timestamp-from-worklog-entry)
;;;----------------------------------------------------------------------------


(defun wlh4-timestamp-location-from-worklog-entry (wl-entry)
  "Return WL-ENTRY's :begin location."

  (org-element-property :begin
			(wlh4-timestamp-from-worklog-entry wl-entry)))

(defalias 'ts--l #'wlh4-timestamp-location-from-worklog-entry)
;;;----------------------------------------------------------------------------


(defun wlh4-duration-from-wl-entry (wl-entry)
  "Return the duration from a WL-ENTRY."

   (plist-get (wlh4-worklog-entry-t-props wl-entry) :duration ))

(defalias 'ts--d #'wlh4-duration-from-wl-entry)

(defun wlh4-duration-as-tenths (wl-entry)
  "Return the duration as a floating-point tenths value."

  (let ((dur (ts--d wl-entry))) ; e.g. "1:18" "hour:min"
    (if (string-match +hour:min-re+ dur)
	(let* ((hours (string-to-number (match-string 1 dur)))
	       (mins  (string-to-number (match-string 2 dur))))
	  (/ (+ (* hours 60.0) mins) 60.0))
      (user-error "No string match in `wlh4-duration-as-tenths': %s" wl-entry))))

(defalias 'd--10s #'wlh4-duration-as-tenths)


;;;----------------------------------------------------------------------------


(defun wlh4-ts-value-from-wl-entry (wl-entry)
  "Return the timestamp range as a string from a WL-ENTRY."

  (org-element-property
   :raw-value
   (wlh4-timestamp-from-worklog-entry wl-entry)))

(defalias 'ts--v #'wlh4-ts-value-from-wl-entry)
;;;----------------------------------------------------------------------------


(defun wlh4-ts-values-from-wl-entry (wl-entry)
  "Return the start and end timestamp values from WL-ENTRY."

  (let ((ts-value (wlh4-ts-value-from-wl-entry wl-entry)))
    (save-match-data
      (string-match "^\\[\\(.*\\)\\]--\\[\\(.*\\)\\]$" ts-value)
      (list (match-string 1 ts-value) (match-string 2 ts-value)))))

(defalias 'ts--vs #'wlh4-ts-values-from-wl-entry)
;;;----------------------------------------------------------------------------


(defun ts--begin (wl-entry)
  "Return begin timestamp from WL-ENTRY."

  (first (ts--vs wl-entry)))

(defun ts--end (wl-entry)
  "Return end timestamp from WL-ENTRY."
  (second (ts--vs wl-entry)))
;;;----------------------------------------------------------------------------


(defun ts--t (wl-entry)
  "Return two Lisp timestamps from WL-ENTRY."

  (let ((t1 (ts--begin wl-entry))
	(t2 (ts--end wl-entry)))
    (list (date-to-time t1) (date-to-time t2))))

(defun ts--t1 (wl-entry)
  "Return first Lisp timestamp from WL-ENTRY."

  (first (ts--t wl-entry)))

(defun ts--t2 (wl-entry)
  "Return second Lisp timestamp from WL-ENTRY."

  (second (ts--t wl-entry)))
;;;----------------------------------------------------------------------------


(defun ts--compare (ts1 ts2)
  "Compare two Lisp timestamp values TS1 and TS2."

  (time-less-p ts1 ts2))
;;;----------------------------------------------------------------------------


(defun wlh4-case-from-worklog-entry (wl-entry)
  "Return the case from WL-ENTRY."

  (first
   (wlh4-worklog-entry-headlines wl-entry)))

(defalias 'c--e #'wlh4-case-from-worklog-entry)

(defun wlh4-rest-headlines-from-worklog-entry (wl-entry)
  "Return a list of the headlines without the case from WL-ENTRY."

  (rest (wlh4-worklog-entry-headlines wl-entry)))

(defalias 'hls--e #'wlh4-rest-headlines-from-worklog-entry)
;;;----------------------------------------------------------------------------


(defun wlh4-time-element-from-wl-entry (wl-entry time-el)
  "Get a TIME-EL from a WL-ENTRY.

TIME-EL can be one of:
- :year-start
- :year-end
- :month-start
- :month-end
- :day-start
- :day-end
- :hour-start
- :hour-end
- :minute-start
- :minute-end"

  (org-element-property time-el
			(wlh4-timestamp-from-worklog-entry wl-entry)))

(defalias 't--e #'wlh4-time-element-from-wl-entry)
;;;----------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPARISON FUNCTION FOR SORTING
(cl-defun wlh4-worklog-entry-compare (a b)
  "Compare two wl-entries for sorting purposes.

Can sort BY 'time (then case) or 'case (then time).  The
default is to sort by 'time.

Return non-nil when wl-entry A is less then wl-entry B.
Compare the elements in the following order:
- year-start
- month-start
- day-start
- hour-start
- minute-start
- then the ends
- finally by case"

  (condition-case err
      ;; this handles one instance of a malformed timestamp:
      ;; there is no time associated with it, which can
      ;; happen sometimes when editing using the calendar.
      ;; It will set the user into workcases.org at the point
      ;; of error.

      (cond

       ((and (eq *wlh4--by-switch* 'case) (string< (c--e a) (c--e b))))
       ((and (eq *wlh4--by-switch* 'case) (string> (c--e a) (c--e b))) nil)
       ((< (t--e a :year-start)  (t--e b :year-start)))
       ((> (t--e a :year-start)  (t--e b :year-start)) nil)
       ((< (t--e a :month-start) (t--e b :month-start)))
       ((> (t--e a :month-start) (t--e b :month-start)) nil)
       ((< (t--e a :day-start)   (t--e b :day-start)))
       ((> (t--e a :day-start)   (t--e b :day-start)) nil)
       ((< (t--e a :hour-start)  (t--e b :hour-start)))
       ((> (t--e a :hour-start)  (t--e b :hour-start)) nil)
       ((< (t--e a :minute-start)(t--e b :minute-start)))
       ((> (t--e a :minute-start)(t--e b :minute-start)) nil)
       ((< (t--e a :year-end)    (t--e b :year-end)))
       ((> (t--e a :year-end)    (t--e b :year-end)) nil)
       ((< (t--e a :month-end)   (t--e b :month-end)))
       ((> (t--e a :month-end)   (t--e b :month-end)) nil)
       ((< (t--e a :day-end)     (t--e b :day-end)))
       ((> (t--e a :day-end)     (t--e b :day-end)) nil)
       ((< (t--e a :hour-end)    (t--e b :hour-end)))
       ((> (t--e a :hour-end)    (t--e b :hour-end)) nil)
       ((< (t--e a :minute-end)  (t--e b :minute-end)))
       ((> (t--e a :minute-end)  (t--e b :minute-end)) nil)
       ((and (eq *wlh4--by-switch* 'time) (string< (c--e a) (c--e b))))
       ((and (eq *wlh4--by-switch* 'time) (string> (c--e a) (c--e b))) nil)
       (t nil))

     (wrong-type-argument
     (signal (car err)
	     (wlh4-timestamp-location-from-worklog-entry b)))))

(cl-defun wlh4-sort-all-worklog-entries (&key (by 'time))
  "Return a new sorted list of wlh4-all-worklog-entries.

This is  a nondestructive  sort of  `*wlh4-all-worklog-entries*' BY
'time (start  times and  end times).  It  can optionally  sort BY
'case  first,  then 'time.   It  returns  the sorted  list.   See
`wlh4-find-clock-entries-sorted'  for a  function that  uses this
function    and   saves    the   result    into   the    variable
`*wlh4-all-worklog-entries-sorted*'."

  (setq *wlh4--by-switch* (cond
		  ((eq by 'time) 'time)
		  ((eq by 'case) 'case)
		  (t (user-error "Wrong sort key: `'%s'" by))))

  (condition-case err

      (seq-sort #'wlh4-worklog-entry-compare *wlh4-all-worklog-entries*)

    (wrong-type-argument
     (switch-to-buffer "workcases.org")
     (goto-char (cdr err))
     (org-reveal)
     (recenter-top-bottom)
     (message "%s: %s" (error-message-string err) (cdr err)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun wlh4-worklog-entries (wl-entries wl-print-fn)
  "Print all WL-ENTRIES to a buffer using WL-PRINT-FN.

WL-ENTRIES is either `*wlh4-all-worklog-entries*' or
`*wlh4-all-worklog-entries-sorted*'."

  (with-temp-buffer-window "*WL-ENTRIES*" nil nil
    (dolist (wl-entry wl-entries)
      (funcall wl-print-fn wl-entry))))

(defun wlh4-worklog-dailies (&optional org-buf case start end)
  "Print worklog daily logs between START timestamp and END timestamp.

Timestamps are  optional, and  if given, day  and month  are also
optional;  the missing  values  will be  filled  in with  default
values.  Optionally only print  wl-entries for CASE.  Use ORG-BUF
or current buffer when interactive."

  (interactive "i\nsCase: \nsStart date: \nsEnd date: ")
  (unless org-buf (setq org-buf (current-buffer)))
  (wlh4-find-clock-entries-sorted org-buf)
  (save-current-buffer
    (let ((prior-file-path "")
	  (cur-buf "")
	  ;; all printed dates must fall between start-datetime and end-datetime
	  (start-datetime (date-to-time (concat (wlh4--fill-in-dates start 'beginning) "T00:00")))
	  (end-datetime (date-to-time (concat (wlh4--fill-in-dates end 'ending) "T23:59"))))
      (dolist (wl-entry *wlh4-all-worklog-entries-sorted*)
	(let ((cur-ts (ts--t1 wl-entry))
	      (cur-case (c--e wl-entry))
	      (cur-file-path (wlh4--wl-daily-file-path wl-entry)))
	  ;; check for matching case (unless empty) and date to be within range
	  ;; skip otherwise
	  (when (and
		 (if (not (string-empty-p case)) (string= cur-case case) t)
		 (ts--compare start-datetime cur-ts)
		 (ts--compare cur-ts end-datetime))
	    ;; when changing dates, save the buffer into a daily worklog
	    ;; and open a new daily worklog buffer
	    (unless (string= prior-file-path cur-file-path)
	      (unless (string-empty-p prior-file-path)
		(save-buffer)
		(kill-buffer cur-buf))
	      (setq prior-file-path cur-file-path)
	      ;; open up a new buffer attached to a file
	      (setq cur-buf (find-file-noselect cur-file-path))
	      (set-buffer cur-buf))
	    ;; finally print the wl-entry into the buffer
	    (wlh4--print-wl-entry wl-entry))))
      (save-buffer)
      (kill-buffer))))

(define-key org-mode-map (kbd "C-c w") 'wlh4-worklog-dailies)

(defun wlh4--wl-daily-file-path (wl-entry)
  "Return the worklog daily filename path for WL-ENTRY.

The two environment variables `WORKLOG' and `COMP' need to be set.
The form of the return string is `${WORKLOG}/worklog.year-month-day.${COMP}.otl'"

  (let ((year (t--e wl-entry :year-start))
	(month (t--e wl-entry :month-start))
	(day (t--e wl-entry :day-start))
	(comp (or (downcase (getenv "COMP"))
		  (user-error "Environment variable `COMP' is not set")))
	(wl-dir (or (getenv "WORKLOG")
		    (user-error "Environment variable `WORKLOG' is not set"))))
    (format "%s/worklog.%s-%02d-%02d.%s.otl" wl-dir year month day comp)))

(defun wlh4--print-wl-entry (wl-entry)
  "Print an individual WL-ENTRY..

This duplicates an entry for worklog.YEAR.otl."

  (let ((case (c--e wl-entry))
	(time-start
	 (format-time-string "%FT%R:00"
			     (org-time-string-to-time (ts--begin wl-entry))))
	(time-end
	 (format-time-string "%FT%R:00"
			     (org-time-string-to-time (ts--end   wl-entry))))
	;; NOTE: there might be more than one headline; this prints only the first
	;;       consider way to print all of them
	(subject (upcase (or (second (wlh4-worklog-entry-headlines wl-entry)) "EMPTY")))
	(detail (wlh4-worklog-entry-detail wl-entry))
	(fill-column 79))
    ;; this duplicates almost perfectly the `worklog.YEAR.otl' format
    ;; TODO: need to figure out a way to include `verb'
    (princ (format "%s\n\t%s\n\t\t%s --- %s\n\t\t\t%s\n%s\n %s\n%s\n%s\n\n"
		   time-start
		   case
		   subject
		   "VERB"
		   "TIME"
		   +wlh4--line+
		   detail
		   +wlh4--line+
		   time-end)
	   (current-buffer))

    (when (> (length detail) 78)
      ;; fill the `detail' when it exceeds one line (78 characters)
      (re-search-backward (concat +wlh4--line+ "\n.*\n" +wlh4--line+))
      (forward-line)
      (fill-region (point) (line-end-position) 'left)
      (goto-char (point-max)))))

(defun wlh4--list-wl-entry (wl-entry)
  (let* ((case (c--e wl-entry))
	 (hls (hls--e wl-entry))
	 (detail (wlh4-worklog-entry-detail wl-entry))
	 (ts-begin (ts--begin wl-entry))
	 (ts-end (ts--end wl-entry))
	 (date (substring-no-properties ts-begin 0 14))
	 (tr-begin (substring ts-begin 15))
	 (tr-end   (substring ts-end   15))
	 (dur (ts--d wl-entry))
	 (tenths (d--10s wl-entry))
	 (loc (ts--l wl-entry)))
    (princ
     (format "%s | %s | %s--%s | %s (%0.2f) [%s]\n%s\n%s\n %s\n\n"
	     case
	     date
	     tr-begin
	     tr-end
	     dur
	     tenths
	     loc
	     hls
	     +wlh4--line+
	     (substring detail 0 (if (> (length detail) 78) 78 (length detail)))))))

(defun wlh4--fill-in-dates (date type)
  "Add default components to a partial DATE.

TYPE should be either 'beginning or 'ending.  When a full date is
provided, it  is returned unchanged.   When a year and  month are
provided, it is  returned with either the beginning  or ending of
the month, as determined by TYPE.   When just a year is provided,
either the first day or the last day of the year are returned."

  (unless (or (eq type 'beginning)
	      (eq type 'ending))
    (user-error "Incorrect TYPE given"))
  (if date
      ;; a non-nil date was supplied; process it
      (if (string-match +date-re+ date)
	  (let* ((len (length (match-data)))
		 (mnth (ignore-errors (string-to-number (match-string 2 date))))
		 (year (string-to-number (match-string 1 date)))
		 (return-date
		  (format "%s%s" date
			  (cond
			   ((eql len 4)
			    (if (eq type 'beginning)
				"-01-01"
			      (if (eq type 'ending)
				  "-12-31")))
			   ((eql len 6)
			    (if (eq type 'beginning)
				"-01"
			      (if (eq type 'ending)
				  (format "-%02d"
					  (calendar-last-day-of-month mnth year)))))
			   ((eql len 8) "")))))
	    return-date)
	(user-error "Incorrect DATE"))
    ;; date is nil, so return earliest or latest date (today)
    (cond
     ((eq type 'beginning) "2004-09-01")
     ((eq type 'ending) (format-time-string "%F" (current-time))))))

(provide 'wlh4-utils)
;;; wlh4-utils.el ends here
