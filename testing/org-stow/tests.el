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
       (:file           ,filename
	  :dir          org-stow:th:examples-dir
	  :visited-name 'tmp
	  :mutable      t)
       (org-mode)
       (let
	  ((org-id-track-globally nil)
	     (org-id-locations '())
	     (org-id-files (list (buffer-file-name))))
	  ,@body)))
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


;;;_ , org-stow-source-children
(emt:deftest-3
   ((of 'org-stow-source-children))
   (nil
      (org-stow:th:in-buf "find-items.org"
	 (emt:doc "Situation: In a buffer with items of known id and children.")
	 (emt:doc "Situation: At an item with no children, no path.")
	 (org-id-open "bfbe7dbc-e3ba-4dd8-9803-65d667e42aaf")
	 (let*
	    ((src (org-stow-source-at-point))
	       (children
		  (org-stow-source-children src))
	       (grandchildren
		  (org-stow-source-children (car children))))
	    
	    (emt:doc "Next generation is length 1 because headline
   starts out contributing one element to hidden-path")
	    (emt:assert (equal (length children) 1))
	    (emt:assert (equal grandchildren nil)))

	 (emt:doc "Situation: At an item with no children but a path.")
	 (org-id-open "23a98b6d-6f90-4808-9c34-0fbdf348fbd6")
	 (let*
	    ((src (org-stow-source-at-point))
	       (children
		  (org-stow-source-children src))
	       (grandchildren
		  (org-stow-source-children (car children)))
	       (greatgrandchildren
		  (org-stow-source-children (car grandchildren))))
	    
	    (emt:doc "Next generation is length 1 because headline
   starts out contributing one element to hidden-path")
	    (emt:assert (equal (length children) 1))
	    (emt:assert (equal (length grandchildren) 1))
	    (emt:assert (equal greatgrandchildren nil)))

	 (emt:doc "Situation: At an item with one child and no path.")
	 (org-id-open "b10e878b-44ab-4d29-ba3c-627a3d598caf")
	 (let*
	    ((src (org-stow-source-at-point))
	       (children
		  (org-stow-source-children src))
	       (grandchildren
		  (org-stow-source-children (car children)))
	       (greatgrandchildren
		  (org-stow-source-children (car grandchildren))))
	    
	    (emt:doc "Next generation is length 1 because headline
   starts out contributing one element to hidden-path")
	    (emt:assert (equal (length children) 1))
	    (emt:assert (equal (length grandchildren) 1))
	    (emt:assert (equal greatgrandchildren nil)))

	 (emt:doc "Situation: At an item with one child and a path.")
	 (org-id-open "02890e58-cc5f-4f0a-a2b9-d5e8a617b0fc")
	 (let*
	    ((src (org-stow-source-at-point))
	       (children
		  (org-stow-source-children src))
	       (grandchildren
		  (org-stow-source-children (car children)))
	       (greatgrandchildren
		  (org-stow-source-children (car grandchildren)))
	       (great^2grandchildren
		  (org-stow-source-children (car greatgrandchildren))))
	    
	    (emt:doc "Next generation is length 1 because headline
   starts out contributing one element to hidden-path")
	    (emt:assert (equal (length children) 1))
	    (emt:assert (equal (length grandchildren) 1))
	    (emt:assert (equal (length greatgrandchildren) 1))
	    (emt:assert (equal great^2grandchildren nil)))

	 (emt:doc "Situation: At an item with two children and no path.")
	 (org-id-open "fd09390d-89a5-4de2-ab7f-45e4c43b8b56")
	 (let*
	    ((src (org-stow-source-at-point))
	       (children
		  (org-stow-source-children src))
	       (grandchildren
		  (org-stow-source-children (car children)))
	       (greatgrandchildren
		  (org-stow-source-children (car grandchildren))))
	    
	    (emt:doc "Next generation is length 1 because headline
   starts out contributing one element to hidden-path")
	    (emt:assert (equal (length children) 1))
	    (emt:assert (equal (length grandchildren) 2))
	    (emt:assert (equal greatgrandchildren nil)))
	 )))



;;;_ , org-stow-goto-location
(emt:deftest-3
   ((of 'org-stow-goto-location))
   (nil
      (org-stow:th:in-buf "stowables-1.org"
	 (emt:doc "Situation: In a buffer with items of known id and path.")
	 (emt:doc "Operation: Go to the position that path indicates.")
	 (emt:doc "Param: Path is a buffer path")
	 (org-stow-goto-location
	    '("buf:" "Target tree"))
	 (emt:assert
	    (equal
	       (org-id-get (point))
	       "6cebd1a3-435b-43c6-80f8-ea863cd57310")))))


;;;_ , org-stow-item
(emt:deftest-3
   ((of 'org-stow-item)
      (db-id org-stow:th:db-id))
   (nil
      (org-stow:th:in-buf "stowables-1.org"
	 (emt:doc "Situation: In a buffer with stowables and the target tree.")
	 (emt:doc "Operation: Stow the ordinary item.")
	 (org-id-open "36da67f8-3fbd-4d72-ae21-78942c2f44ec")
	 (org-stow-item)
	 (emt:assert
	    (emt:eq-persist-p #'equal 
	       (org-stow:th:id->tree-string
		  "6cebd1a3-435b-43c6-80f8-ea863cd57310")
	       "dbid:aa4f964d-1dae-4b11-96ef-fc171e0d6f51"))

	 (emt:doc "Operation: Stow a second item.")
	 (org-id-open "2aa5968e-8566-43b1-905c-fa602866230e")
	 (org-stow-item)
	 (emt:assert
	    (emt:eq-persist-p #'equal 
	       (org-stow:th:id->tree-string
		  "6cebd1a3-435b-43c6-80f8-ea863cd57310")
	       "dbid:746766cf-4e19-4782-8dbd-052182260a34"))))
   
   (nil
      (org-stow:th:in-buf "stowables-1.org"
	 (emt:doc "Situation: In a buffer with stowables and the target tree.")

	 (emt:doc "Operation: Stow the second ordinary item.")
	 (org-id-open "2aa5968e-8566-43b1-905c-fa602866230e")
	 (org-stow-item)
	 
	 (emt:assert
	    (emt:eq-persist-p #'equal 
	       (org-stow:th:id->tree-string
		  "6cebd1a3-435b-43c6-80f8-ea863cd57310")
	       "dbid:48d71bb4-0bb8-468d-92f1-07abf35da2bd"))

	 (emt:doc "Operation: Stow the note competing with second note.")
	 (org-id-open "264615d9-17d5-42bf-8a6e-1aeb7af9cca3")
	 (emt:assert
	    (emth:gives-error
	       (org-stow-item)))
	 
	 (emt:doc "Operation: Unstow the second ordinary item.")
	 (org-stow-unstow-item)
	 ;;Back to original buffer

	 (emt:doc "Operation: Again try to stow the note competing
	 with second note.")
	 (progn
	    (org-id-open "264615d9-17d5-42bf-8a6e-1aeb7af9cca3")
	    (org-stow-item)
	    (emt:assert
	       (emt:eq-persist-p #'equal 
		  (org-stow:th:id->tree-string
		     "6cebd1a3-435b-43c6-80f8-ea863cd57310")
		  "dbid:f53810d7-ab9d-4786-b035-e0ebe6ce7f30")))


	 ))


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
