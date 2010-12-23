;;; ob-ledger.el --- org-babel functions for ledger evaluation

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Eric S Fraga
;; Keywords: literate programming, reproducible research, accounting
;; Homepage: http://orgmode.org
;; Version: 7.4

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating ledger entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in ledger
;;
;; 2) we are generally only going to return output from the leger program
;;
;; 3) we are adding the "cmdline" header argument
;;
;; 4) there are no variables

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:ledger
  '((:results . "output") (:cmdline . "bal"))
  "Default arguments to use when evaluating a ledger source block.")

(defvar org-babel-ledger-command "ledger"
  "Command to invoke ledger")

(defun org-babel-execute:ledger (body params)
  "Execute a block of Ledger entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (org-babel-eval
   (concat org-babel-ledger-command " -f - " (cdr (assoc :cmdline params)))
   body))

(defun org-babel-prep-session:ledger (session params)
  (error "Ledger does not support sessions"))

(provide 'ob-ledger)

;; arch-tag: 7bbb529e-95a1-4236-9d29-b0000b918c7c

;;; ob-ledger.el ends here
