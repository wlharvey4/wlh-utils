;;; wlh4-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;; Author: wlh4
;; Initial Commit: 2021-03-10
;; Time-stamp: <2021-04-12 06:56:41 lolh-mbp-16>
;; Version: 0.5.8



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


;; USAGE: wlh4-defs <RET> [elisp-buffer]
;;        (wlh4-def [elisp-buffer])
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


;;; wlh4-org-tree-traversal:
;;; ------------------------

;; Walk an Org Tree using the Preorder Traversal method
;; See https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/GenTreeIntro.html

;; COMMAND: wlh4-walk-org-tree ORG-BUF
;; USAGE:   M-x wlh4-walk-org-tree <RET> buffer
;;          (wlh4-walk-org-tree "walk.org")

(defun wlh4-walk-org-tree (org-buf)
  "Command to walk (traverse) an OrgTree of an Org buffer `buf'."

  (interactive "bBuffer to parse: ")
  (with-temp-buffer-window "*OrgTree*" nil nil
    (wlh4-org-tree-traversal (wlh4-parse-org-buffer org-buf) 0)))



;; Some utility functions
(defun wlh4-parse-org-buffer (buf)
  "Utility to parse an Org-mode buffer into an OrgTree."
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
  "Performs a `preorder' traversal of an OrgTree.

This function  traverses an OrgTree  starting from the  root node
obtained        from        the       org-element        function
`org-element-parse-buffer'.   An OrgTree  node  (`OrgNode') is  a
recursive list data structure containing either:

- type: a `type' designator identifying the type of node
- props: a plist of properties relevant to the type, and 
- contents: an indefinite number of child OrgNodes,

or

- a secondary string representing a plain-text element;

Thus, an OrgNode is one of:

-  OrgNode: (<type> (plist ...) (child OrgNode) (child OrgNode) ...)
or
-  OrgNode: #(\"string\" # # plist ...)

	    (debug)
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





;;; wlh4-find-clock-entries
;; Use the procedure  of traversing an Org Tree  but work specifically
;; on Clock entries only.

(defvar wlh4-all-worklog-entries
  "List by line position of wlh4-worklog-entry elements.")

(defvar wlh4-all-worklog-entries-sorted
  "Sorted list (by either  time or case-time) of wlh4-worklog-entry
elements.")

(cl-defstruct wlh4-worklog-entry
  "Structure to hold a worklog entry."

  headlines 	; (list of strings)
  detail 	; (string)
  c-props 	; (plist of common properties)
  t-props) 	; (plist of clock properites)

(defconst ts-re--inactive
  ;;"\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)\\)"
  "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +[^]+0-9>\r\n -]+ +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)\\]"

  "A regular expression representing an inactive timestamp.
This  is taken  from `org-ts-regexp0'  in `org.el'  but with  all
subexpressions eliminated and all optional parts made required.")

(defconst tsr-re--inactive
  (concat ts-re--inactive "--" ts-re--inactive)

  "A regular expression representing an inactive timestamp range.")


;;  TODO: option to run on full org-buf or only visible portion
;;        right now it runs only on visible portion
(defun wlh4-find-clock-entries (&optional org-buf display)
  "Wrapper for main routine; user will select ORG-BUF.

With non-nil optional DISPLAY, do  display results of search to a
temporary buffer."
  (interactive)
  (unless org-buf (setq org-buf (current-buffer)))
  (setf wlh4-all-worklog-entries nil) ; start fresh
  (catch 'clock-problem
    (with-current-buffer org-buf
      (if display
	(with-temp-buffer-window "*OrgClocks*" nil nil
	  (wlh4-traverse-clock-entries (wlh4-parse-org-buffer org-buf) 0 'display))
	(wlh4-traverse-clock-entries (wlh4-parse-org-buffer org-buf) 0))))
  t)

(cl-defun wlh4-find-clock-entries-sorted (org-buf &key (by 'time))
  "Wrapper for main routine to use ORG-BUF and sort the list.

By default,  the sort is done  only BY time.  It  will optionally
sort BY  case first, then  time.  This routine stores  the sorted
entries,  and does  not  return anything  nor  print anything  to
temporary buffer."

  (interactive "bBuffer")
  (wlh4-find-clock-entries org-buf)
  (setq wlh4-all-worklog-entries-sorted
	(wlh4-sort-all-worklog-entries :by by))
  t)


(defun extract--common-keys (all-props)
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
	  (extract--common-keys props)

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
	  (let* ((ts-ok (string-match tsr-re--inactive rv))
		 (min1 (mod (string-to-number (or (match-string 6 rv) "0")) 6))
		 (min2 (mod (string-to-number (or (match-string 6 rv) "0")) 6))
		 (dur-hr (progn (string-match "\\(^[[:digit:]]\\{1,2\\}\\):" dur)
				(string-to-number (match-string 1 dur))))
		 (clock-problem
		  (cond
		   ((not ts-ok) "Malformed timestamp")
		   ((string= stat "running") "Running clock")
		   ((string= dur "0:00") "Zero duration")
		   ((> dur-hr 3) "Extended duration")
		   ((null tag) "Missing tag")
		   ((string-empty-p detail) "Missing detail")
		   (t nil))))
	    (when clock-problem
	      (print clock-problem)
	      (message "%s: %s" clock-problem t-props)
	      (switch-to-buffer (current-buffer))
	      (goto-char (plist-get c-props :begin))
	      (org-reveal)
	      (throw 'clock-problem tse))
	    (when
		(or (cl-plusp min1) (cl-plusp min2))
	      (goto-char (1+ (org-element-property :begin tse)))
	      (unless (zerop min1)
		(org-timestamp-change (- min1) 'minute))
	      (unless (zerop min2)
		(search-forward "]--[")
		(org-timestamp-change (- 6 min2) 'minute))))

	  ;; III. Store the clock before continuing traversal
	  ;;      All necessary information is in these four items
	  (push (make-wlh4-worklog-entry
		 :headlines headlines ; the first list item is the case
		 :detail detail       ; string telling what was done
		 :t-props t-props     ; contains the timestamp and duration
		 :c-props c-props)    ; can be used to locate the clock
		wlh4-all-worklog-entries))))

    ;; IV. Traverse the org-tree
    (when (listp contents)
      (let ((child (first contents))
	    (children (rest contents)))
	(while child
	  (wlh4-traverse-clock-entries child (1+ level) display)
	  (setf child (first children))
	  (setf children (rest children))))))

  t)



;;; ACCESSOR FUNCTIONS
(defun wlh4-timestamp-from-worklog-entry (wl-entry)
  "Return the full timestamp from a WL-ENTRY."

  (plist-get (wlh4-worklog-entry-t-props wl-entry) :value))

(defalias 'ts--e #'wlh4-timestamp-from-worklog-entry)



(defun wlh4-timestamp-location-from-worklog-entry (wl-entry)
  "Return WL-ENTRY's :begin location."

  (org-element-property :begin
			(wlh4-timestamp-from-worklog-entry wl-entry)))

(defalias 'ts--l #'wlh4-timestamp-location-from-worklog-entry)



(defun wlh4-duration-from-wl-entry (wl-entry)
  "Return the duration from a WL-ENTRY."

   (plist-get (wlh4-worklog-entry-t-props wl-entry) :duration ))

(defalias 'ts--d #'wlh4-duration-from-wl-entry)

(defun wlh4-ts-value-from-wl-entry (wl-entry)
  "Return the timestamp from a WL-ENTRY."

  (org-element-property
   :raw-value
   (wlh4-timestamp-from-worklog-entry wl-entry)))

(defalias 'ts--v #'wlh4-ts-value-from-wl-entry)

(defun wlh4-ts-values-from-wl-entry (wl-entry)
  "Return the start and end timestamp values from WL-ENTRY."

  (let ((ts-value (wlh4-ts-value-from-wl-entry wl-entry)))
    (save-match-data
      (string-match "^\\[\\(.*\\)\\]--\\[\\(.*\\)\\]$" ts-value)
      (list (match-string 1 ts-value) (match-string 2 ts-value)))))

(defalias 'ts--vs #'wlh4-ts-values-from-wl-entry)

(defun ts--begin (wl-entry)
  "Return begin timestamp from WL-ENTRY."

  (first (ts--vs wl-entry)))

(defun ts--end (wl-entry)
  "Return end timestamp from WL-ENTRY."
  (second (ts--vs wl-entry)))

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

(defun ts--compare (ts1 ts2)
  "Compare two Lisp timestamp values."

  (time-less-p ts1 ts2))

(defun wlh4-case-from-worklog-entry (wl-entry)
  "Return the case from WL-ENTRY."

  (first
   (wlh4-worklog-entry-headlines wl-entry)))

(defalias 'c--e #'wlh4-case-from-worklog-entry)

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

(defvar wlh4--by 'time)

;;; COMPARISON FUNCTION FOR SORTING
;; TODO: use Org time calculations here instead
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

       ((and (eq wlh4--by 'case) (string< (c--e a) (c--e b))))
       ((and (eq wlh4--by 'case) (string> (c--e a) (c--e b))) nil)
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
       ((and (eq wlh4--by 'time) (string< (c--e a) (c--e b))))
       ((and (eq wlh4--by 'time) (string> (c--e a) (c--e b))) nil)
       (t nil))

    (wrong-type-argument
     (signal (car err)
	     (wlh4-timestamp-location-from-worklog-entry b)))))

(cl-defun wlh4-sort-all-worklog-entries (&key (by 'time))
  "Return a new sorted list of wlh4-all-worklog-entries.

This is a nondestructive sort of `wlh4-all-worklog-entries' BY
time (start times and end times).  It can optionally sort BY case
first, then times.  It returns the sorted list.  See
`wlh4-find-clock-entries-sorted' for a function that uses the
function but saves the result into the variable
`wlh4-all-worklog-entries-sorted'."

  (setq wlh4--by (cond
		  ((eq by 'time) 'time)
		  ((eq by 'case) 'case)
		  (t (user-error "Wrong sort key: `'%s'" by))))

  (condition-case err

      (seq-sort #'wlh4-worklog-entry-compare wlh4-all-worklog-entries)

    (wrong-type-argument
     (switch-to-buffer "workcases.org")
     (goto-char (cdr err))
     (org-reveal)
     (recenter-top-bottom)
     (message "%s: %s" (error-message-string err) (cdr err)))))

(defun wlh4-worklog-entries (wl-entries)
  "List all WL-ENTRIES.

WL-ENTRIES is either `wlh4-all-worklog-entries' or
`wlh4-all-worklog-entries-sorted'."

  (with-temp-buffer-window "*WL-ENTRIES*" nil nil
    (dolist (wl-entry wl-entries)
      (princ (format "(%s) %s ==> %s  <%s>\n"
		     (c--e wl-entry)
		     (ts--v wl-entry)
		     (ts--d wl-entry)
		     (ts--l wl-entry))))))

;; TODO: use (org-with-point-at POM BODY)
;; TODO: use (org-with-wide-buffer BODY)
;; TODO: use (org-wrap S &opt WIDTH LINES)
;; TODO: use (org-add-props)
(defun wlh4-check-all-worklog-entries-sorted ()
  "Iterate through sorted list to check for overlaps."

  (with-temp-buffer-window "*OVERLAPS*" nil nil
      (setq overlaps nil
	    wl (make-wlh4-worklog-entry )
	    t1 '(0 0)
	    t2 '(0 0))
      (dolist (wl-entry wlh4-all-worklog-entries-sorted overlaps)
	(let ((ts-t1 (ts--t1 wl-entry))
	      (ts-t2 (ts--t2 wl-entry)))
	  (when (ts--compare ts-t1 t2)
	    (push wl overlaps)
	    (push wl-entry overlaps)
	    (princ (format "%s\n%s\n\n" wl wl-entry)))
	  (setq wl wl-entry
		t1 ts-t1
		t2 ts-t2))))
  (wlh4-worklog-entries (reverse overlaps)))

(defun wlh4-worklog-entries-tsr-verify (&optional org-buf)
  "Verify the integrity of the buffer's timestamp ranges."

  (interactive)
  (unless org-buf (setq org-buf (current-buffer)))
  (wlh4-find-clock-entries-sorted org-buf)
  (setq c 0
	wl nil
	prior-wl-entry nil
	overlapped-wl-entry nil)
  (let ((final 
	 (catch 'end-verify
	   (catch 'overlap
	     (catch 'dur
	       (catch 'ts
		 (dolist (wl-entry wlh4-all-worklog-entries-sorted)
		   (cond ((verify--timestamp-range wl-entry) (setq wl wl-entry) (throw 'ts wl-entry))
			 ((verify--duration-value wl-entry) (setq wl wl-entry) (throw 'dur wl-entry))
			 ((verify--overlapping-times wl-entry) (setq wl wl-entry) (throw 'overlap wl-entry))
			 (t (cl-incf c)(princ (format "[%s]" c)))))
		 (throw 'end-verify 'Verified))
	       (message "%s\n%s" "Timestamp" wl)
	       (terpri)
	       (verify--display-incorrect-wl-entry wl)
	       (throw 'end-verify 'Timestamp))
	     (message "%s\n%s" "Duration" wl)
	     (terpri)
	     (verify--display-incorrect-wl-entry wl)
	     (throw 'end-verify 'Duration))
	   (prin1 (format "%s" "Overlap."))
	   (terpri)
	   (verify--display-incorrect-wl-entry wl overlapped-wl-entry)
	   (throw 'end-verify 'Overlap))))
    (message "%s" final)))

(defun verify--timestamp-range (wl-entry)
  "Verify that a timestamp range is well-formed and properly rounded."
  (let ((tsr (wlh4-ts-value-from-wl-entry wl-entry)))
    ;; must return nil when there is a good match
    ;; must return t when there is not a good match
    (or
     (null (string-match tsr-re--inactive tsr))
     (let* ((min1 (mod (string-to-number (match-string 3 tsr)) 6))
	    (min2 (mod (string-to-number (match-string 6 tsr)) 6)))
       (not (and (zerop min1) (zerop min2)))))))

(defun verify--duration-value (wl-entry)
  ;; must return t there is a zero duration
  (let* ((dur (ts--d wl-entry))
	 (min (progn
		(string-match "\\([[:digit:]]\\{1,2\\}\\):\\([[:digit:]]\\{2\\}\\)" dur)
		(string-to-number (match-string 2 dur)))))
    (or
     (string= dur "0:00")
     (> (mod min 6) 0))))
  
(defun verify--overlapping-times (wl-entry)
  "Return t if a begin time comes before a prior end time."

  (let* ((tsr (wlh4-ts-value-from-wl-entry wl-entry))
	 (begin-ts (progn
		     (string-match tsr-re--inactive tsr)
		     (match-string 1 tsr)))
	 (prior-tsr (if prior-wl-entry
			(wlh4-ts-value-from-wl-entry prior-wl-entry)
		      "[2004-09-07 Tue 0:00]--[2004-09-07 Tue 0:00]"))
	 (prior-end-ts (progn
			 (string-match tsr-re--inactive prior-tsr )
			 (match-string 4 prior-tsr))))
    (setq overlapped-wl-entry (copy-wlh4-worklog-entry prior-wl-entry))
    (setq prior-wl-entry (copy-wlh4-worklog-entry wl-entry))
    (org-time< begin-ts prior-end-ts)))

(defun verify--display-incorrect-wl-entry (wl &optional wl2)
  (switch-to-buffer "workcases.org")
  (goto-char (ts--l wl))
  (when wl2
    (switch-to-buffer-other-window "workcases.org")
    (goto-char (ts--l wl2))))

(defun wlh4-round-timestamp-range ()
  "Round an inactive timestamp range to multiples of tenths of hour.

This  command assumes  point is  on a  left bracket  beginning an
inactive timestamp range.  It will report an error otherwise."

  (interactive)
  (let ((tsr (looking-at tsr-re--inactive)))
    ;; (string-match tsr-re--inactive tsr)
    (let ((t1 (match-beginning 1))
	  (t2 (match-beginning 5))
	  (min1 (- (mod (string-to-number (match-string 4)) 6)))
	  (min2 (- 6 (mod (string-to-number (match-string 8)) 6))))
      (forward-char)
      (org-timestamp-change min1 'minute)
      (goto-char t2)
      (unless (= min2 6)
	(org-timestamp-change min2 'minute 'updown)))))

;;; wlh4-utils.el ends here
