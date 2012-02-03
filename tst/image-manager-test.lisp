;;;; image-manager-test.lisp
(in-package :image-manager-test)

(define-test image-string-transformation-suite
  (dotimes (i 2)
    (with-magick-wand
        (test-image-wand
         :load
         (namestring
          "/home/b-man/Projects/captcha-data/seed/0/0_AvantGarde-Book.jpg"))
      (print (get-wand-raw-data! test-image-wand))
      ;; (pre-process-wand! test-image-wand)
      ;; (draw-random-line! test-image-wand)
      ;; (magick-write-image test-image-wand
      ;;                     (namestring (asdf:system-relative-pathname
      ;;                                     'captcha
      ;;                                     (format nil "data/tst/~a.jpg" i))))
      )))

;; have a look at the data/tst/ directory to check out the images.
(run-tests image-string-transformation-suite)