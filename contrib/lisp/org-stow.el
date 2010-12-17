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
DEPTH-DELTA is the difference in depth."
   (let*
      ((prop-drawer-points
	  (org-get-property-block))
	 ;;Get beginning of the entry proper.
	 ;;org-property-end-re
	 (parts
	    (append
	       (list
		  ;;New headline, maybe indented
		  ;;differently.
		  (make-string 
		     (+ (org-current-level) depth-delta)
		     ?*)
		  " "
		  (org-get-heading)
		  "\n")
				     
	       ;;Properties block, with any id
	       ;;transformed.
	       ;;$$IMPROVE ME - leave out ID lines.
	       (when prop-drawer-points
		  (list
		     "    :PROPERTIES:\n"
		     (buffer-substring
			(car prop-drawer-points)
			(cdr prop-drawer-points))
		     "    :END:\n"))
	       ;;The rest, to the end of text entry.
	       (when prop-drawer-points
		  (list
		     (buffer-substring
			(cdr prop-drawer-points)
			(org-entry-end-position)))))))
      parts))

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
