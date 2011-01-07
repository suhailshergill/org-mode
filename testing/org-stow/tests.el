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

;;;_ , org-stow-get-item-copy

;;Go to item 7ff68f58-0115-45b2-8c3c-ef245b90081f
;;Check that the copy is what we want.
;;Might loop over examples.


;;;_ , org-dblock-write:stowed-into
(emt:deftest-3
   ((of 'org-dblock-write:stowed-into)
      (db-id org-stow:th:db-id))
   (nil
      (emtb:with-buf
	 (:file   "dyn-blocks.org"
	    :dir     org-stow:th:examples-dir
	    :mutable t)
	 (org-mode)
	 (emt:doc "Situation: In a file with our type of dynamic blocks.")
	 (emt:doc "Operation: Update all the dynamic blocks.")
	 (org-dblock-update t)
	 (emt:doc "Result: As we expect.")
	 (emt:assert
	    (emt:eq-persist-p #'equal
	       (buffer-substring-no-properties (point-min) (point-max))
	       "dbid:169cba75-6164-4f25-8672-fa6b6c90989e")))))


;;;_ , org-stow-dblock-action
;;Find:
'"stowed-into :id 7ff68f58-0115-45b2-8c3c-ef245b90081f"
;;Check that the result is what we expect.


;;;_. Footers
;;;_ , Provides

(provide 'org-stow/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-stow/tests.el ends here
