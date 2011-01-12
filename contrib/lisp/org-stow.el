;;;_ org-stow.el --- Stow org items

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: outlines,convenience,tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;; 


;;;_ , Requires

(require 'org)

;;;_. Body
;;;_ , Types
(defstruct (org-stow-target
	      (:constructor org-stow-make-target)
	      (:conc-name org-stow-target->))
   
   "A target object"
   (type      () :type (member dblock item virtual))
   (path      () :type (repeat string))
   (rv-virt-path () :type (repeat string)))


(defstruct (org-stow-source
	      (:constructor org-stow-make-source)
	      (:conc-name org-stow-source->))
   "A source object"
   (id          () :type symbol)
   (hidden-path () :type (repeat string))
   (headline    () :type string))
;;;_ , Constants
;;;_  . org-stow-unwanted-props
(defconst org-stow-unwanted-props 
   '("ID" "CATEGORY" "STOW-TO" "STOW-HIDDEN-PATH")
   "Properties that shouldn't appear in item copies" )
;;;_ , Utility
;;;_  . org-stow-map-single-level
(defun org-stow-map-single-level (depth func &optional match scope &rest other-skips)
   ""
   
   (let
      ((skipfunc
	  ;;Function to restrict to this depth.  `org-agenda-skip'
	  ;;undocumentedly requires return value (if not nil) to be a
	  ;;count or marker that it can go to.
	  `(lambda ()
	      (if
		 (equal 
		    (org-reduced-level (org-current-level))
		    ,depth)
		 nil
		 (point)))))
      
      (apply #'org-map-entries
	 func match scope skipfunc other-skips)))
;;;_  . org-stow-search-dblock-start
(defun org-stow-search-dblock-start ()
   "Search the start of the dblock point is in."
   (unless
      (looking-at org-dblock-start-re)
      (re-search-backward org-dblock-start-re)))
;;;_  . org-stow-search-dblock-end
(defun org-stow-search-dblock-end ()
   "Search the end of the dblock point is in."
   (re-search-forward org-dblock-end-re nil t))

;;;_  . org-stow-dblock-params
(defun org-stow-dblock-params ()
   "Return the parameters of the dblock point is in.
The :name parameter is given as well.
Assumes point is in a dblock."
   (save-excursion
      (org-stow-search-dblock-start)
      (let
	 (  (start (point))
	    (name (org-no-properties (match-string 1)))
	    (raw-params (read (concat "(" (match-string 3) ")"))))

	 (append 
	    (list 
	       :name name
	       :start start
	       :end
	       (save-excursion
		  (org-stow-search-dblock-end)
		  (point)))
	    raw-params))))
;;Test this on a known file

;;;_ , Mirroring
;;;_  . org-stow-get-item-copy
(defun org-stow-get-item-copy (depth-delta)
   "Return a list of strings that when inserted are a copy of current item.
DEPTH-DELTA is the difference in depth.
The id property, if it exists, will be changed to source-id."
   (let*
      ((prop-drawer-points
	  (org-get-property-block))
	 ;;$$IMPROVE ME  Remove tags "stowable" and "stowed"
	 (rest-of-heading (org-get-heading))
	 (depth
	    (org-current-level))
	 (beginning-entry-proper
	    (if
	       prop-drawer-points
	       (save-excursion
		  (goto-char (cdr prop-drawer-points))
		  (search-forward-regexp org-property-end-re)
		  (match-end 0))
	       (save-excursion
		  (org-back-to-heading t)
		  (if (looking-at "\\*+[ \t]+\\([^\r\n]*\\)")
		     (match-end 0) 
		     (point)))))
	 (end-entry
	    (org-entry-end-position))
	 (properties
	    (org-entry-properties nil 'standard))
	 (id-prop
	    (assoc "ID" properties))
	 ;;Remove certain properties
 	 (properties
	    (delq nil
	       (mapcar
		  #'(lambda (prop)
		       (if (member (car prop) org-stow-unwanted-props)
			  '()
			  prop))
		  properties)))
	 ;;Add source-id to properties if we can.
	 (properties
	    (if id-prop
	       (cons
		  (cons "SOURCE-ID" (cdr id-prop))
		  properties)
	       properties)))

      ;;Make the new item, as a list of strings.
      `(
	  ;;New headline, maybe indented differently.
	  ,(make-string 
	      (+ depth depth-delta)
	      ?*)
	  " "
	  ;;The rest of the heading.
	  ,rest-of-heading
	  "\n"

	  ;;New properties block.
	  ,@(if properties
	       `(  "    :PROPERTIES:\n"
		   ,@(apply #'nconc 
		      (mapcar
			 #'(lambda (prop)
			      (list
				 "    :"
				 (car prop)
				 ": "
				 (cdr prop)
				 "\n"))
			 properties))
		   "    :END:\n")
	       '())
	 
	  ;;The rest, to the end of text entry.
	  ,(buffer-substring
	      beginning-entry-proper
	      end-entry))))

;;;_  . org-dblock-write:stowed-into
(defun org-dblock-write:stowed-into (params)
   "Make a dblock behave somewhat like a symlink"
   (let* (  
	    (m-depth (plist-get params :depth))
	    (id (plist-get params :source-id))
	   (text-list
	      (save-excursion
		 (org-id-goto id)
		 (let*
		    ((subst-depth (org-current-level))
		       (depth-delta (- m-depth subst-depth)))
		    (apply #'nconc
		       (org-map-entries
			  #'(lambda ()
			       (org-stow-get-item-copy depth-delta))
			  nil
			  'tree)))))
	    
	    (inhibit-read-only t))

      (mapcar 
	 #'(lambda (text)
	      (insert (propertize text 'read-only t)))
	 text-list)))

;;;_ , Making sources and targets
;;;_  . org-stow-apair
(deftype org-stow-apair ()
   ""
   '(list string (repeat org-stow-source) (or null org-stow-target)))
;;;_  . org-stow-get-split-arglists
(defun org-stow-get-split-arglists (sources target-parent)
   "Return a list of data each suitable for `org-stow-get-actions-aux'.
SOURCES must be a list of `org-stow-source'.
TARGET-PARENT must be an `org-stow-target'."

   ;;Collect unique headlines' data as (headline source-list target)
   (let*
      ((alist '()))
      (dolist (src sources)
	 (let*
	    ((headline (org-stow-source->headline src))
	       (apair (assoc headline alist)))
	    ;;Add source to the relevant apair in alist, creating it
	    ;;if needed.
	    (if apair
	       (progn
		  (check-type apair org-stow-apair)
		  (push src (second apair)))
	       (push
		  (list headline (list src) nil)
		  alist))))
      
      ;;If target is an item, add its children as targets, overriding
      ;;any defaults.
      (when
	 (eq (org-stow-target->type target-parent) 'item)
	 (dolist (trgt (org-stow-get-target-children target-parent))
	    (let*
	       (
		  (rv-virt-path
		     (org-stow-target->rv-virt-path trgt))
		  (headline 
		     (if rv-virt-path
			(car rv-virt-path)
			(last (org-stow-target->path trgt))))
		  (apair (assoc headline alist)))
	       ;;Add target to the relevant apair in alist, creating it
	       ;;if needed.
	       (if apair
		  (progn
		     (check-type apair org-stow-apair)
		     ;;Check that targets have unique headlines.  This
		     ;;may be rethought.
		     (assert (null (third apair)))
		     (setf (third apair) trgt))
		  (push
		     (list headline '() trgt)
		     alist)))))

      ;;$$IMPROVE ME Short-circuit for apairs with no sources.
      (mapcar
	 #'(lambda (apair)
	      (check-type apair org-stow-apair)
	      ;;Return is (source-list target)
	      (if
		 (third apair)
		 ;;If a target is known, use it.
		 (cdr apair)
		 ;;Otherwise make one.
		 (list 
		    (second apair)
		    (org-stow-make-target
		       :type 'virtual
		       :path (org-stow-target->path target-parent)
		       :rv-virt-path
		       (cons
			  (first apair)
			  (org-stow-target->rv-virt-path target-parent))))))
	 alist)))

;;Test this: Make sources and targets from a known file.  Recognize results.

;;;_  . org-stow-path->target
(defun org-stow-path->target (path)
   "Return a target object, given a path."
   ;;$$IMPROVE ME  Check that path is non-empty
   ;;$$IMPROVE ME  Check that the item can be found.
   (org-stow-make-target
      :path path
      :rv-virt-path '()
      :type 'item))

'(org-stow-path->target 
       (org-entry-get-multivalued-property (point) "STOW-TO"))

;;;_  . org-stow-get-target-children
(defun org-stow-get-target-children (target)
   "Return a list of the children of TARGET, including dblocks.
If target is not an item, return an empty list."

   (if
      (eq (org-stow-target->type target) 'item)
      (save-excursion
	 (org-stow-goto-location (org-stow-target->path target))
	 (let
	    ((start (org-entry-beginning-position))
	       (end
		  (org-end-of-subtree t))
	       (depth
		  (org-reduced-level (org-current-level)))
	       (parent-path (org-stow-target->path target)))
	    
	    ;;Narrow so that we only see dynamic blocks within this
	    ;;item.
	    (save-restriction
	       (narrow-to-region start end)
	       ;;Loop over dynamic blocks within restriction
	       (let*
		  (
		     (dblocks-data
		      (let 
			 ((collected '()))
			 (org-map-dblocks
			    #'(lambda ()
				 (let
				    ((params
					(org-stow-dblock-params)))
				    ;;Our type of dblock
				    (when 
				       (equal 
					  (plist-get params :name) 
					  "stowed-into")
				    (push params collected)))))
			 (nreverse collected)))
		     (dblock-targets
			(mapcar
			   #'(lambda (params)
				(org-stow-make-target
				   :type 'dblock
				   :rv-virt-path 
				   (list (plist-get params :headline))
				   :path  parent-path))
			   dblocks-data))
		     ;;Each region we inspect begins at start or the
		     ;;end of a dblock and ends at the start of the
		     ;;next dblock or end.
		     (start-posns
			(cons 
			   start
			   (mapcar 
			      #'(lambda (params)
				   (plist-get params :end))
			      dblocks-data)))
		     (end-posns
			(nconc
			   (mapcar
			      #'(lambda (params)
				   (plist-get params :start)) 
			      dblocks-data) 
			   (list end)))
		     (item-targets
			(apply #'nconc
			   (mapcar*
			      #'(lambda (start end)
				   (save-restriction
				      (narrow-to-region start end)
				      (org-stow-map-single-level
					 (1+ depth)
					 #'(lambda ()
					      (org-stow-path->target
						 (append parent-path 
						    (list 
						       (nth 4
							  (org-heading-components)))))))))
			      start-posns
			      end-posns))))

		  (append item-targets dblock-targets)))))
      
      ;;Otherwise return the empty list
      '()))

;;To test we'd like a function that describes a target object's type
;;and headline: (last (org-stow-target->path target))
'(org-stow-get-target-children 
    (org-stow-get-target 
       (org-entry-get-multivalued-property (point) "STOW-TO")))

;;;_  . org-stow-source-at-point
(defun org-stow-source-at-point (&optional immediate)
   "Return a source object corresponding to item at point.

If IMMEDIATE is nil or not given, make headline field nil and
append real headline to hidden-path."
   (let*
      (
	 ;;The path from the target ancestor to the first target node
	 ;;that's realized in the source.  Unused for now.
	 (hidden-path
	    (org-entry-get-multivalued-property (point)
	       "STOW-HIDDEN-PATH"))
	 (id
	    (org-id-get-create))
	 (headline
	    (nth 4 (org-heading-components)))
	 (hidden-path
	    (if immediate
	       hidden-path
	       (append hidden-path (list headline))))
	 (headline
	    (if immediate headline nil)))

      (org-stow-make-source 
	 :id id
	 :hidden-path hidden-path
	 :headline headline)))
;;;_  . org-stow-source-children
(defun org-stow-source-children (source)
   "Return a list of children of SOURCE.  
If it has no children, raise error."
   
   (let
      ((hidden-path
	  (org-stow-source->hidden-path source)))
      (if hidden-path
	 (list
	    (org-stow-make-source 
	       :id (org-stow-source->id source)
	       :hidden-path (cdr hidden-path)
	       :headline (car hidden-path)))
	 (progn
	    ;;Go to that item.
	    (org-id-goto
	       (org-stow-source->id source))

	    ;;Traverse the subtree
	    (let*
	       ((depth
		   (org-reduced-level (org-current-level))))
	       (org-stow-map-single-level (1+ depth)
		  #'(lambda ()
		       (org-stow-source-at-point t))
		  nil
		  'tree))))))

;;Test by checking `org-stow-source-at-point' on the children.  And
;;`org-stow-source-at-point' to get the original argument, too.

;;;_ , Finding actions
;;;_  . org-stow-get-actions-item
(defun org-stow-get-actions-item (source-list target)
   "Get a list of actions to stow SOURCE-LIST in a normal item"

   ;;$$IMPROVE ME If any source has text for this item and no hidden
   ;;path, error.
   (let*
      ;;Expand each source to a list of its children
      ((source-children
	  (apply #'nconc
	     (mapcar
		#'(lambda (src)
		     ;;$$IMPROVE ME Maybe also assert that src, being
		     ;;an inner node, had no text.  Just if it has no
		     ;;hidden path.
		     (let* 
			((children (org-stow-source-children src)))
			(unless children
			   (error 
			      "Tree could not be stowed, couldn't \
split it far enough."))
			children))
		
		source-list)))
	 (child-groups
	    (org-stow-get-split-arglists source-children target)))

      ;;Recurse as needed
      (apply #'nconc
	 (mapcar
	    #'(lambda (group)
		 (apply #'org-stow-get-actions group))
	    child-groups))))
;;;_  . org-stow-get-actions-dblock+single
(defun org-stow-get-actions-dblock+single (src target)
   ""
   ;;$$IMPROVE ME Check whether ids match.  If not, replace the dblock
   ;;even if there's no hidden path..  It is possible for this to
   ;;happen if a source item went away without being unstowed.
      
   ;;$$IMPROVE ME if source has a hidden path, instead make
   ;;`dblock->item', zero or more `create-item', and
   ;;`create-mirror'
   `((already-present
	,target
	,(org-stow-target->depth     target)
	(org-stow-source->headline  src))))

;;;_  . org-stow-get-actions-dblock
(defun org-stow-get-actions-dblock (source-list target)
   "Get a list of actions to stow SOURCE-LIST in a dblock"
   (let*
      ((params 
	  (save-excursion
	     (org-stow-goto-location 
		(org-stow-target->path target))
	     (org-stow-dblock-params)))
	    
	 (source-id (plist-get params :source-id))
	 (headline  (plist-get params :headline)))
      (if (= (length source-list) 0)
	 ;;If we were called with empty SOURCE-LIST, which
	 ;;sometimes happens, then we know the tree needs no
	 ;;splitting or other action.  
	 '()
	 ;;If we were called with singleton source and it matches the
	 ;;dblock source, then we're just tidying up.
	 (if (and
		(= (length source-list) 1)
		(equal source-id 
		   (org-stow-source->id (car source-list))))
	    (org-stow-get-actions-dblock+single 
	       (car source-list)
	       target)
	    
	    ;;Otherwise we must split the subtree.
	    (cons
	       `(dblock->item
		   ,target
		   ,(org-stow-target->depth target)
		   ,headline)
	       (org-stow-get-actions-item 
		  ;;$$IMPROVE ME if id is the same as an existing
		  ;;source, don't add as a new source.

		  ;;The dblock's id indicates another source that
		  ;;contributes to this subtree, so include it.
		  (cons
		     (org-stow-make-source 
			:id source-id
			:headline headline)
		     source-list) 
		  target))))))
;;;_  . org-stow-target->depth
(defun org-stow-target->depth (target)
   ""
   (+
      (length (org-stow-target->path         target))
      (length (org-stow-target->rv-virt-path target))
      -1))
;;;_  . org-stow-get-actions-virtual
;;$$TEST ME with making nested stuff
(defun org-stow-get-actions-virtual (source-list target)
   "Get a list of actions to stow SOURCE-LIST in a virtual item"
   (if (= (length source-list) 1)
      (let
	 ((src (car source-list)))
	 `((create-mirror
	      ,target
	      ,(org-stow-source->id        src)
	      ,(org-stow-source->headline  src)
	      ,(org-stow-target->depth     target))))
      
      (org-stow-get-actions-item source-list target)))

;;;_  . org-stow-get-actions
(defun org-stow-get-actions (source-list target)
   "Get a list of actions to stow SOURCE-LIST in TARGET"
   (funcall
      (ecase (org-stow-target->type target)
	 (item #'org-stow-get-actions-item)
	 (dblock #'org-stow-get-actions-dblock)
	 (virtual #'org-stow-get-actions-virtual))
      source-list
      target))
;;;_ , Finding locations
;;;_  . org-stow-goto-location
(defun org-stow-goto-location (location)
   "Move point to target LOCATION."
   ;;Location's representation may change.  Path is fundamental, but
   ;;may be augmented.
   (let*
      (  (buf-p (string= (car location) "buf:"))
	 (mark (if buf-p
		  (org-find-olp (cdr location) t)
		  (org-find-olp location))))
      (set-buffer (marker-buffer mark))
      (goto-char mark)
      (let
	 ((pblock (org-get-property-block)))
	 (when pblock 
	    (goto-char (cdr pblock))
	    (next-line)))
      (set-marker mark nil)))
;;;_  . org-stow-goto-target
(defun org-stow-goto-target (target)
   ""
   
   (let*
      (  (type (org-stow-target->type target))
	 (rv-virt-path
	    (org-stow-target->rv-virt-path target))
	 (dheadline
	    (if (eq type 'dblock)
	       (car rv-virt-path)
	       nil))
	 (rv-virt-path
	    (if (not (eq type 'item))
	       (cdr rv-virt-path)
	       rv-virt-path))
	 (path
	    (append
	       (org-stow-target->path target)
	       (reverse rv-virt-path)))
	 (pt nil))
      
      (org-stow-goto-location path)
      ;;Now find the dblock, if applicable.
      (when (eq type 'dblock)
	 (org-map-dblocks
	    #'(lambda ()
		 (let* 
		    ((params
			(org-stow-dblock-params)))
		    ;;If a dblock matches, we'll use its position as
		    ;;point.
		    (when
		       (equal
			  (plist-get params :headline)
			  dheadline)
		       (setq pt (point))))))
	 (when pt (goto-char pt)))
      ;;Otherwise find a place after the end of the subtree
      (when (eq type 'virtual)
	 (org-end-of-subtree))))




;;;_ , Actions
;;;_  . Individual actions
;;;_   , org-stow-create-item
(defun org-stow-create-item (depth headline)
   "Create an item with the given DEPTH and HEADLINE."
   (apply #'insert
      ;;$$FACTOR ME with `org-stow-get-item-copy'
      `(
	  ,(make-string depth ?*)
	  " "
	  ,headline
	  "\n")))
;;Test on an empty org buffer

;;;_   , org-stow-create-mirror
(defun org-stow-create-mirror (source-id headline depth)
   "Create a mirror with the given SOURCE-ID and HEADLINE"
   (org-create-dblock 
      (list
	 :name "stowed-into" 
	 :source-id source-id
	 :headline headline
	 :depth depth)))
;;Test on an empty org buffer

;;;_   , org-stow-erase-dblock
(defun org-stow-erase-dblock ()
   "Erase the dblock at this location."
   ;;$$IMPROVE ME Assert that it's one of ours.
   ;;$$IMPROVE ME Assert that point is indeed within a dblock, ie
   ;;finding a dblock beginning from the end finds the start we found,
   ;;and vv.
   ;;
   (let*
      (  (start
	    ;;$$ENCAP ME
	    (save-excursion
	       (org-stow-search-dblock-start)
	       (point)))
	 (end
	    ;;$$ENCAP ME
	    (save-excursion
	       (org-stow-search-dblock-end)
	       (point))))
      (delete-region start end)))
;;Test on a known file

;;;_   , org-stow-dblock->item
(defun org-stow-dblock->item (depth headline)
   "Replace a dblock with an item of DEPTH and HEADLINE."
      (org-stow-erase-dblock)
      (org-stow-create-item depth headline))
;;Test on a known file

;;;_  . org-stow-do-action
(defun org-stow-do-action (governor target &rest args)
   "Dispatch an action"
   (save-excursion
      (org-stow-goto-target target)
      (apply
	 (ecase governor
	    (create-item   #'org-stow-create-item)
	    (create-mirror #'org-stow-create-mirror)
	    (dblock->item  #'org-stow-dblock->item)
	    (already-present #'ignore))
	 args)))


;;;_ , org-stow-item
(defun org-stow-item ()
   "Stow the current item.
Ie, make it (a dynamic copy of it and its subtree) appear in another place."
   
   (interactive)
   (let*
      ((target-path
	  ;;An path to the parent of the target.
	  (org-entry-get-multivalued-property (point) "STOW-TO"))
	 ;;$$IMPROVE ME  Check that item has a target-path, otherwise
	 ;;raise error.
	 (source
	    (org-stow-source-at-point))
	 (target (org-stow-path->target target-path))
	 (actions
	    (org-stow-get-actions (list source) target)))

      ;;If we got here, we encountered no error, so commit all the
      ;;actions.
      (dolist (act actions)
	 (apply #'org-stow-do-action act))

      ;;Mark this item "stowed".
      (org-toggle-tag "stowable" 'off)
      (org-toggle-tag "stowed" 'on)))


;;;_ , org-stow-unstow-item
;;$$WRITE ME
;;$$TEST ME
(defun org-stow-unstow-item ()
   "Unstow the current item.
Ie, remove a dynamic copy of it, if there is one."
   ;;This might use the same code as `org-stow-item', except finding
   ;;actions in a different way and setting different tags.  So factor
   ;;that part out.
   (interactive)
   (let*
      ()
      ;;Again find all the locations
      ;;Erase our dblocks there (We own them or they'd appear as conflicts)

      ;;Remove tag "stowed" (back to "stowable")
      (org-toggle-tag "stowed"   'off)
      (org-toggle-tag "stowable" 'on)))


;;;_ , org-stow-make-item-stowable 
(defun org-stow-make-item-stowable ()
   "Make the current item stowable."
   
   (interactive)
   (let*
      ;;$$IMPROVE ME Find the set of target files from file marks,
      ;;customizable variables, etc.

      ;;Interactively find the parent of its target.  We presume it's
      ;;stored immediately underneath the target, since storing it as
      ;;the target makes little sense.  Gives (path filename regexp
      ;;position).
      ((location
	  (let ((org-refile-targets '((nil . (:maxlevel . 10))))
		  (org-refile-use-outline-path 'full-file-path))
	     (org-refile-get-location "Parent item: ")))
	 ;;Split the path into a list.
	 (path-raw
	    (split-string 
	       (car location) "/"))
	 ;;Convert filename to foreslashes
	 (filename-1
	    (mapconcat
	       #'identity
	       (split-string (car path-raw) "\\\\")
	       "/"))
	 (path (cons filename-1 (cdr path-raw))))
      
      ;;Store path in multivalued property STOW-TO
      (apply 
	 #'org-entry-put-multivalued-property
	 (point)
	 "STOW-TO"
	 path)
      ;;Add a tag "stowable"
      (org-toggle-tag "stowable" 'on)))


;;;_ , Add an item to a stowable subtree
;;;_ , Integration with org-choose

;;Let choosing (when it moves items to or from a "satisfied" level)
;;call unstowing and stowing.

;;;_. Footers
;;;_ , Provides

(provide 'org-stow)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-stow.el ends here
