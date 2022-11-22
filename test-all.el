(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (if (not (string-equal f "test-all.el"))
		                   (load-file (concat (file-name-as-directory dir) f))))))
	  (mapc load-it (directory-files-recursively dir "\\.el$"))))

(load-directory "./")
