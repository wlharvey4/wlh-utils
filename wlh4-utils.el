;;; wlh4-utils.el --- Utilities for elisp libraries  -*- mode:emacs-lisp -*-

;; Author: wlh4
;; Initial Commit: 2021-03-10
;; Time-stamp: <2021-04-06 08:31:29 lolh-mbp-16>
;; Version: 0.5.1



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
;; Wrapper for wlh4-clock-entries
;;
;; wlh4-clock-entries:
;; -------------------
;; Procedure to walk an org tree using the preorder traversal method
;; and extract and parse all clock entries.  Report errors for certain
;; problems, like a running clock, a zero duration, an entry without
;; a case tag.



;;; Code:

(require 'cl-lib)
(require 'seq)

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

(defun _pc-props (props indent)
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
      (_pc-props t-props level-indent))
    (when (consp c-props) ; print the common properties
      (_pc-props c-props level-indent))
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




(defvar wlh4-all-worklog-entries
  "List of wlh4-worklog-entry elements.")

(cl-defstruct wlh4-worklog-entry
  "Structure to hold a worklog entry."

  headlines 	; (list strings)
  detail 	; (string)
  c-props 	; (plist)
  t-props) 	; (plist)

;;; wlh4-find-clock-entries
;;  TODO: option to run on full org-buf or only visible portion
;;        right now it runs only on visible portion
(defun wlh4-find-clock-entries (org-buf)
  "Wrapper for main routine; user will select ORG-BUF."
  (interactive "bBuffer")
  (setf wlh4-all-worklog-entries nil) ; start fresh
  (catch 'clock-problem
    (with-current-buffer org-buf
      (with-temp-buffer-window "*OrgClocks*" nil nil
	(wlh4-clock-entries (wlh4-parse-org-buffer org-buf) 0)))))


(defun _extract-common-keys (all-props)
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


;;; wlh4-clock-entries
;; Main routine to parse clock elements for worklog information.
(defun wlh4-clock-entries (org-node level)
  "Recursively extract all clock ORG-NODE's from an org buffer.

Keep track of the current LEVEL during recursion."

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
	  (_extract-common-keys props)

	;; find timestamp, duration, status, raw-value, tags, headings
	(let* ((ts (plist-get t-props :value))     ; clock's timestamp value
	       (dur (plist-get t-props :duration)) ; clock's duration
	       (stat (plist-get t-props :status))  ; clock's status
	       (rv (org-element-property :raw-value ts)) ; ts's raw value
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

       	  (princ
	   (format "%s: {%s} %s %s (%s)\n%s\n%s\n  %s\n  %s\n\n"
		   type tag rv dur stat headlines detail c-props t-props))

	  ;; Find and report some serious clocking problems.
	  ;; Will open a second window into the buffer and
	  ;; place the cursor at the problem clock with a
	  ;; message.
	  (let ((clock-problem
		 (cond
		  ((string= stat "running") "Running clock")
		  ((string= dur "0:00") "Zero duration")
		  ((null tag) "Missing tag")
		  ((string-empty-p detail) "Missing detail")
		  (t nil))))
	    (when clock-problem
	      (print clock-problem)
	      (message "%s: %s" clock-problem t-props)
	      (switch-to-buffer-other-window (current-buffer))
	      (goto-char (plist-get c-props :begin))
	      (org-reveal)
	      (throw 'clock-problem ts)))

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
	  (wlh4-clock-entries child (1+ level))
	  (setf child (first children))
	  (setf children (rest children))))))

  ;; Return the global variable full of wlentry items
  t)



;;; ACCESSOR FUNCTIONS
(defun wlh4-timestamp-from-worklog-entry (wl-entry)
  "Return the full timestamp from a WL-ENTRY."

  (plist-get (wlh4-worklog-entry-t-props wl-entry) :value))

(defun wlh4-duration-from-wl-entry (wl-entry)
  "Return the duration from a WL-ENTRY."

   (plist-get (wlh4-worklog-entry-t-props wl-entry) :duration ))

(defun wlh4-ts-value-from-wl-entry (wl-entry)
  "Return the timestamp from a WL-ENTRY."

  (org-element-property
   :raw-value
   (wlh4-timestamp-from-worklog-entry wl-entry)))

(defun wlh4-case-from-worklog-entry (wl-entry)
  "Return the case from a WL-ENTRY."

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

;;; COMPARISON FUNCTION FOR SORTING
(defun wlh4-worklog-entry-compare (a b)
  "Compare two wl-entries for sorting purposes.

Return non-nil when wl-entry A is less then wl-entry B.
Compare the elements in the following order:
- year-start
- month-start
- day-start
- hour-start
- minute-start
- then the ends
- finally by case"

  (cond
   ((string< (c--e a) (c--e b)))
   (t nil)))

(defun wlh4-sort-all-worklog-entries ()
  "Return a new sorted list of wlh4-all-worklog-entries.

This is  a nondestructive  sort by start  times, end  times, then
cases. `wlh4-all-worklog-entries' remains unchanged."

  (seq-sort #'wlh4-worklog-entry-compare wlh4-all-worklog-entries))

;;; wlh4-utils.el ends here
