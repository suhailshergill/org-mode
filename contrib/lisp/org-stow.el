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
;;;_ , org-stow-unwanted-props
(defconst org-stow-unwanted-props 
   '("ID" "CATEGORY" "STOW-PATH")
   "Properties that shouldn't appear in item copies" )
;;;_ , org-stow-get-item-copy
(defun org-stow-get-item-copy (depth-delta)
   "Return a list of strings that when inserted are a copy of current item.
DEPTH-DELTA is the difference in depth.
The id property, if it exists, will be changed to source-id."
   (let*
      ((prop-drawer-points
	  (org-get-property-block))
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

;;;_ , org-dblock-write:stowed-into
(defun org-dblock-write:stowed-into (params)
   "Make a dblock behave somewhat like a symlink"
   (let* (  (components (org-heading-components))
	    (m-depth (org-current-level))
	    (id (plist-get params :id))
	   (text-list
	      (save-excursion
		 (org-id-goto id)
		 (let*
		    ((subst-depth (org-current-level))
		       (depth-delta (1+ (- m-depth subst-depth))))
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

;;;_ , org-stow-create-mirror
(defun org-stow-create-mirror (id headline)
   ""

   (let*
      ()
      ;;Do we move to there or assume we are already there?
      '(org-create-dblock 
	  (list
	     :name "stowed-into" 
	     :id id  ;;$$CHANGE MY NAME :source might be better.
	     :headline headline))))


;;Do NOT create heading with (org-insert-heading), it's too high, does
;;too much.  Possibly factor something out of `org-stow-get-item-copy'
;;;_ , org-stow-dblock-params
(defun org-stow-dblock-params ()
   "Return the parameters of the dblock point is in.
The :name parameter is given as well.
Assumes point is in a dblock."
   (unless
      (looking-at org-dblock-start-re)
      (re-search-backward org-dblock-start-re))
   (let*
      ((name (org-no-properties (match-string 1)))
	 (params (append (list :name name)
		    (read (concat "(" (match-string 3) ")")))))
      params))

;;;_ , org-stow-dblock-action
(defun org-stow-dblock-action (headlines-sought id)
   ""

   (let*
      ((params (org-stow-dblock-params))
	 (headline (plist-get params :headline))
	 (its-id (plist-get params :id)))
      (cond
	 ;;It's some other type of dblock - no action.
	 ((not (equal (plist-get params :name) "stowed-into")) nil)
	 ;;It's our type and the headline is one we seek.
	 ((member headline headlines-sought)
	    (if
	       (equal id its-id)
	       ;;It's our own link.  We don't need to do anything
	       ;;more here.
	       '()
	       ;;It's a link to another item.  We will have to split
	       ;;the tree so both are in place.  $$TRANSITIONAL We'll
	       ;;probably need to add more info to this form.
	       `(split ,headline ,id ,its-id)))
	 ;;It's our type, but not a headline we're interested in - no
	 ;;action.
	 (t nil))))

;;;_ , org-stow-get-actions
;;;_ , org-stow-item
(defun org-stow-item ()
   "Stow the current item.
Ie, make it (a dynamic copy of it and its subtree) appear in another place."
   
   (interactive)
   (let*
      ((stow-to
	  ;;An id that points at an ancestor of the target.
	  (org-entry-get (point) "STOW-TO" nil))
	 ;;The path from the target ancestor to the first target node
	 ;;that's realized in the source.
	 ;;$$PUNT for now
	 (stow-path
	    '(org-entry-get-multivalued-property (point)
		"STOW-PATH"))
	 ;;This node's own id.  It must have one for org-stow to work.
	 (id
	    (org-entry-get (point) "ID" nil))
	 (headlines-sought
	    (list
	       (nth 5 (org-heading-components)))))
      
      ;;Split this off.  It will recurse.
      
      (save-excursion
	 (org-id-goto stow-to)
	 (let
	    ((start (org-entry-beginning-position))
	       (end
		  (org-end-of-subtree t)))
	    
	    ;;$$PUNT Look at the children, step down the prefix path.
	    ;;This can result in at most one conflict.

	    ;;$$PUNT Collect actions, then if there's no conflict, do
	    ;;them.

	    ;;If that item has (non-dynamic) children, visit the children.

	    ;;Narrow so that we only see dynamic blocks within this
	    ;;item.
	    (save-restriction
	       (narrow-to-region start end)
	       ;;Loop over dynamic blocks within that item.  

	       (let
		  ((rv-actions '()))
		  (org-map-dblocks
		     #'(lambda ()
			  (let
			     ((action
				 (org-stow-dblock-action
				    headlines-sought
				    id)))
			     (when action
				(push action rv-actions)))))
		  ;;$$IMPROVE ME  Quit early if we found as many items
		  ;;as we sought.

		  ;;Now find the children.
		  
		  )
	       
	       ))

	 
	 
	 )
      

      
      ))


;;Find that location (by id or headline)  The id will point at the
;;  *parent* of our root.  NB, the lower ones can't have real ids
;;  because they might well live in notes, which shouldn't become our target.
;;Is it available?  Use it or recurse.  We must recurse thru our prefix.
;;Recursing, use headlines?  Can't safely use ids, they might point
;;anywhere. 
;;Collect what to do.
;;If there are any conflicts, report them.
;;Otherwise create dblocks in all the places.  Create our ids if they
;;don't already exist.
;;Add a tag "stowed"
;;org-use-property-inheritance or not?
;;;_ , org-unstow-item
(defun org-unstow-item ()
   "Unstow the current item.
Ie, remove a dynamic copy of it, if there is one."
   
   (interactive)
   (let*
      ()
      
      ))
;;Again find all the locations
;;Erase our dblocks there (We own them or they'd appear as conflicts)
;;Remove tag "stowed" (back to "stowable")

;;;_ , org-stow-make-item-stowable 
(defun org-stow-make-item-stowable ()
   "Make the current item stowable."
   
   (interactive)
   (let*
      ()
      
      ;;Find its target
      ;;Get that id and a prefix.
      ;;Store that prefix in property stow-path, a multivalued property.
      ;;Add a tag "stowable"

      '(org-entry-put-multivalued-property pom property &rest values)
      ))

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
