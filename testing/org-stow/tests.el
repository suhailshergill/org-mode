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

;;;_. Body
;;;_ , Insulation
(defconst org-stow:th:surrounders 
   '()
   "The normal surrounders for org-stow tests" )
;;;_ , org-stow-get-item-copy
;;;_ , org-dblock-write:stowed-into
;;Find:
'"stowed-into :id 7ff68f58-0115-45b2-8c3c-ef245b90081f"
;;In other tests, just go to that file.

;;;_ , org-stow-dblock-action
;;Find:
'"stowed-into :id 7ff68f58-0115-45b2-8c3c-ef245b90081f"



;;;_. Footers
;;;_ , Provides

(provide 'org-stow/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org-stow/tests.el ends here
