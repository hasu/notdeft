;; Full path of "notdeft-xapian" executable. An alternative would be
;; to use `notdeft-xapian-make-program-when-uncurrent' to compile the
;; executable and configure it as `notdeft-xapian-program'.
(let ((x
       (let ((default-directory
	       (file-name-directory
		(file-truename (locate-library "notdeft")))))
	 (file-truename "xapian/notdeft-xapian"))))
  (setq notdeft-xapian-program x))
