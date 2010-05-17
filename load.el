;;;_ load.el --- Set up load-path for org

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

;; This file just sets up load-path.  It's usable by `my-site-start',
;; just symlink it into ~/.emacs.d/site-start.d/

;;;_ , Requires

;; Nothing.

;;;_. Body

(add-to-list 'load-path
   (expand-file-name "lisp"
      (if load-file-name
	 (file-name-directory
	    (file-truename load-file-name)))))

(add-to-list 'load-path
   (expand-file-name "contrib/lisp"
      (if load-file-name
	 (file-name-directory
	    (file-truename load-file-name)))))

(add-to-list 'load-path
   (expand-file-name "testing"
      (if load-file-name
	 (file-name-directory
	    (file-truename load-file-name)))))

(add-to-list 'Info-default-directory-list
   (expand-file-name "doc"
      (if load-file-name
	 (file-name-directory
	    (file-truename load-file-name)))))

;;;_. Footers
;;;_ , Provides

;;(Is not a package)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + End:

;;;_ , End
;;; load.el ends here

