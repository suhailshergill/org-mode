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
;;;_ , org-stow-get-item-copy
(defun org-stow-get-item-copy (depth-delta)
   "Return a list of strings that when inserted are a copy of current item.
DEPTH-DELTA is the difference in depth.
The id property, if it exists, will be changed to source-id."
   (let*
      ((prop-drawer-points
	  (org-get-property-block))
	 (rest-of-heading (org-get-heading))
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

	 (properties
	    (org-entry-properties nil 'standard))
	 (id-prop
	    (assoc "ID" properties))
	 ;;Remove certain properties
 	 (properties
	    (delq nil
	       (mapcar
		  #'(lambda (prop)
		       (cond
			  ((equal (car prop) "ID") nil)
			  ((equal (car prop) "CATEGORY") nil)
			  (t prop)))
		  properties)))
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
	      (+ (org-current-level) depth-delta)
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
	      (org-entry-end-position)))))

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
		       (depth-delta (- subst-depth m-depth 1)))
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

;;;_ , Creating them.

'
(org-create-dblock 
   (list
      :name "stowed-into" 
      :id 'source-ID
      :headline "Headline"))

;;NOT (org-insert-heading), it's too high, does too much.

;;;_ , Stowing a subtree

;;Find prefix
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

;;;_ , Unstowing
;;Again find all the locations
;;Erase our dblocks there (We own them or they'd appear as conflicts)
;;Remove tag "stowed" (back to "stowable")

;;;_ , Mark an item (subtree) stowable
;;Find its target
;;Get that id and a prefix.
;;Store that prefix.
;;Add a tag "stowable"

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
