;;;_ org-stow/tests.el --- Tests for org-stow

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(require 'org-stow)
(require 'org)

(require 'emtest/main/define)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/persist)
(require 'emtest/testhelp/mocks/filebuf)

;;;_. Body
;;;_ , Insulation
(defconst org-stow:th:surrounders 
   '()
   "The normal surrounders for org-stow tests" )
;;;_ , Filename constants
(defconst org-stow:th:examples-dir
   (emt:expand-filename-here "examples") 
   "Directory where examples are" )
(defconst org-stow:th:db-id 
   `(persist ,(expand-file-name "db" org-stow:th:examples-dir))
   "" )
;;;_ , Helpers
(defmacro org-stow:th:in-buf (filename &rest body)
   ""
   
   `(emtb:with-buf
       (:file   ,filename
	  :dir     org-stow:th:examples-dir
	  :mutable t)
       (org-mode)
       ,@body))


;;;_ , org-stow-get-item-copy

(emt:deftest-3
   ((of 'org-stow-get-item-copy)
      (db-id org-stow:th:db-id))
   (nil
      (org-stow:th:in-buf "dyn-blocks.org"
	 (emt:doc "Situation: In a file with our type of dynamic blocks.")
	 (org-id-open "7ff68f58-0115-45b2-8c3c-ef245b90081f")
	 (emt:doc "Situation: Point is at a known item.")
	 (emt:assert
	    (emt:eq-persist-p 
	       #'equal 
	       (org-stow-get-item-copy 0)
	       "dbid:83a0ec41-f598-4091-b653-8bba0ef89c2c")))))

;;;_ , org-dblock-write:stowed-into
(emt:deftest-3
   ((of 'org-dblock-write:stowed-into)
      (db-id org-stow:th:db-id))
   (nil
      (org-stow:th:in-buf "dyn-blocks.org"
	 (emt:doc "Situation: In a file with our type of dynamic blocks.")
	 (emt:doc "Operation: Update all the dynamic blocks.")
	 (org-dblock-update t)
	 (emt:doc "Result: As we expect.")
	 (emt:assert
	    (emt:eq-persist-p #'equal
	       (buffer-substring-no-properties (point-min) (point-max))
	       "dbid:169cba75-6164-4f25-8672-fa6b6c90989e")))))


;;;_ , org-stow-dblock-action
(emt:deftest-3
   ((of 'org-stow-dblock-action)
      (db-id org-stow:th:db-id))
   (nil
      (org-stow:th:in-buf "dyn-blocks.org"
	 (emt:doc "Situation: In a file with our type of dynamic blocks.")
	 (search-forward
	    "stowed-into :source-id 7ff68f58-0115-45b2-8c3c-ef245b90081f")
	 (emt:doc "Situation: Point is on the sole dynamic block.")
	 (emt:doc "Operation: Find actions.")
	 (emt:doc "Param: HEADLINES-SOUGHT contains (only) the
   headline of the block.")
	 (emt:doc "Param: ID is the id the dblock knows about.")
	 (emt:doc "Response: nil, indicating no action needed.")
	 (emt:assert
	    (emt:eq-persist-p 
	       #'equal 
	       (org-stow-dblock-action
		  '("Headline A")
		  "7ff68f58-0115-45b2-8c3c-ef245b90081f")
	       "dbid:78a627ba-2a39-4a8a-8ff7-823a221fe7f9"))))

   (nil
      (org-stow:th:in-buf "dyn-blocks.org"
	 (emt:doc "Situation: In a file with our type of dynamic blocks.")
	 (search-forward
	    "stowed-into :source-id 7ff68f58-0115-45b2-8c3c-ef245b90081f")
	 (emt:doc "Situation: Point is on the sole dynamic block.")
	 (emt:doc "Operation: Find actions.")
	 (emt:doc "Param: HEADLINES-SOUGHT contains (only) the
   headline of the block.")
	 (emt:doc "Param: ID is a different id than the dblock uses.")
	 (emt:doc "Response: A splitting action.")
	 (emt:assert
	    (emt:eq-persist-p 
	       #'equal 
	       (org-stow-dblock-action
		  '("Headline A")
		  "7ff68f58-different-id")
	       "dbid:78a627ba-2a39-4a8a-8ff7-823a221fe7f9"))))
   )

;;Check that the result is what we expect.
;;;_ , Find an item's children, both dblocks and real items
;;;_ , org-stow-item
;;;_ , org-unstow-item
;;;_ , org-stow-make-item-stowable

;;;_. Footers
;;;_ , Provides

(provide 'org-stow/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-stow/tests.el ends here
