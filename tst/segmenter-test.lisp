;;;; segmenter-test.lisp
(in-package :segmenter-test)

(define-test segmenter-suite
    (with-magick-wand
        (test-wand :load
                   (namestring (asdf:system-relative-pathname
                                'captcha
                                "data/captchas/785794337.jpg")))
      (let ((imgs (image->sweeped-segments test-wand
                                           0.05
                                           0.15
                                           (lambda (raw-data)
                                             '(0 0 0 1)))))
        (assert-equal (length imgs) 19))))

(run-tests segmenter-suite)


