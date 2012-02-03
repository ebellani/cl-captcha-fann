;;;; recognizer-test.lisp
(in-package :recognizer-test)


;; (define-test segments-suite
;;   (let* ((trained-ann
;;           (load-ann-from-file
;;            (namestring 
;;             (asdf:system-relative-pathname
;;              'captcha
;;              "data/networks/default-net-1024-1000-32.net"))))
;;          (segments (image->segments
;;                     (namestring (asdf:system-relative-pathname
;;                                  'captcha
;;                                  "data/captchas/913079854.jpg"))
;;                     trained-ann)))
;;     (dolist (a-segment segments) 
;;       (print a-segment))
;;     (assert-equal (length segments) 19)
;;     (assert-true  (typep (car segments) 'segment))))

;; (run-tests segments-suite)






