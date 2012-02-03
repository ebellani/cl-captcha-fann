;;;; captcha-test.lisp

(in-package :tester-test)

(define-test trained-ann-suite
  (let ((trained-ann
         (load-ann-from-file
          "/home/b-man/Projects/captcha-data/fann-3kkimgs-50epochs-750neuros.net")))
    (print (test-ann-on-image-tree
            trained-ann
            "/home/b-man/Projects/captcha-data/letras_cortadas/"))))

;; (run-tests trained-ann-suite)

