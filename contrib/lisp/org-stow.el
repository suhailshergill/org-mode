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
;;;_ , org-dblock-write:stowed-into
(defun org-dblock-write:stowed-into (params)
   (let* ((id (plist-get params :id))
	    ;;$$IMPROVE ME Figure out how deep the block should be.
	   (text-list
	      (save-excursion
		 (org-id-goto id)
		 (org-map-entries
		    #'(lambda ()
			 (buffer-substring
			    (org-entry-beginning-position)
			    (org-entry-end-position)))
		    nil
		    'tree)))
	    
	    (inhibit-read-only t))
      ;;$$IMPROVE ME Remove ids from property blocks, replace them
      ;;with a different property that links back to the source.
      ;;$$IMPROVE ME Change the number of stars in the entry.  Need to
      ;;know current depth.
      ;;$$IMPROVE ME Make this read-only.
      ;;$$IMPROVE ME Somehow make todo changes etc affect the source.
      ;;They (correctly) don't affect the read-only text.  Or at least
      ;;put point back there.
      ;;$$IMPROVE ME Somehow make changes in the source propagate back
      ;;- but for now, manually redoing all dynamic blocks will
      ;;suffice. 
      ;;$$IMPROVE ME Map over entries.  Use org-map-entries with scope
      ;;`tree'.
      (mapcar 
	 #'(lambda (text)
	      (insert (propertize text 'read-only t)))
	 text-list)))

;;;_ , Creating them.

;;`org-create-dblock'
;;;_ , Stowing a subtree

;;;_. Footers
;;;_ , Provides

(provide 'org-stow)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-stow.el ends here
