;; load.el
;; Added by me (Tehom) to make loading usable by `my-site-start'

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


