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
;;;_ , org-stow:th:id->tree-string
(defun org-stow:th:id->tree-string (id)
   "Return the buffer-substring of the tree at ID in current buffer."
   
   (save-excursion
      (org-id-open id)
      (let
	 ((kill-ring '()))
	 (org-copy-subtree)
	 (car kill-ring))))


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


;;;_ , Find an item's children, both dblocks and real items
;;;_ , org-stow-item
(emt:deftest-3
   ((of 'org-stow-item)
      (db-id org-stow:th:db-id))
   (nil
      (org-stow:th:in-buf "stowables-1.org"
	 (emt:doc "Situation: In a file with our type of dynamic blocks.")
	 (emt:doc "Operation: Stow the ordinary item.")
	 (org-id-open "36da67f8-3fbd-4d72-ae21-78942c2f44ec")
	 (org-stow-item)
	 (emt:assert
	    (emt:eq-persist-p #'equal 
	       (org-stow:th:id->tree-string
		  "6cebd1a3-435b-43c6-80f8-ea863cd57310")
	       "dbid:aa4f964d-1dae-4b11-96ef-fc171e0d6f51"))

	 (org-id-open "2aa5968e-8566-43b1-905c-fa602866230e")
	 (org-stow-item)
	 (emt:assert
	    (emt:eq-persist-p #'equal 
	       (org-stow:th:id->tree-string
		  "6cebd1a3-435b-43c6-80f8-ea863cd57310")
	       "dbid:746766cf-4e19-4782-8dbd-052182260a34"))


	 ))

   ;;Stow second item, then Note competing with second note.  Expect
   ;;an error.
   ;;Unstow second item, then stow Note competing with second
   ;;note.

   ;;Stow Note with hidden path, then Note with hidden path 2
   ;;This should make paths

   ;;Test stowing into another file, stowing by id, stowing by current buffer.
   )


;;;_ , org-unstow-item
;;;_ , org-stow-make-item-stowable
;;Needs its own example file and wildcard comparison.  Saving the
;;persisting value won't suffice for this.

;;;_. Footers
;;;_ , Provides

(provide 'org-stow/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-stow/tests.el ends here
