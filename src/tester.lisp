;;;; tester.lisp

;;;; used to test a given FANN captcha neural net with in an test hierarchy.

(in-package :tester)

;; (defvar *test-image-tree*
;;   (asdf:system-relative-pathname 'captcha "data/test-raw-captcha-letters/"))

(defmacro mark-time (variable)
  `(setf ,variable
         (- (/ (get-internal-real-time)
               internal-time-units-per-second) ,variable)))

(defun test-ann-on-image-tree (trained-ann image-tree-pathname
                               &optional (width 16) (height 16))
  "Takes the TRAINED-ANN and runs it on every image found on the
IMAGE-TREE-PATHNAME, returning the percentage of hits and misses."
  (let ((directories-bit-string-hash-table
         (get-bit-string-directories-hash image-tree-pathname))
        (success 0)
        (failure 0)
        (time-elapsed 0))
    (walk-only-valid-directories
     image-tree-pathname
     (lambda (some-pathname)
       (mark-time time-elapsed)
       (let* ((network-return
               (run trained-ann
                    (with-magick-wand
                        (wand :load (namestring some-pathname))
                      (magick-scale-image wand width height)
                      (get-wand-raw-data! wand :float-list))))
              (true-label (last-directory-name some-pathname))
              (guessed-label (bit-list->label network-return)))
         (print "**************************************************")
         (print (format nil "file name ~a" some-pathname))
         (print (format nil "net result ~a" network-return))
         (print (format nil "true label:~a~% guessed label:~a"
                        true-label guessed-label))
         (if (equal (aref true-label 0) guessed-label)
             (incf success)
             (incf failure)))
       (mark-time time-elapsed)))
    (let ((total (+ success failure)))
      (print (format nil "Time per recognition: ~a"
              (float (/ time-elapsed total))))
      (format nil "Successes: ~a~% Failures: ~a"
              (float (/ success total))
              (float (/ failure total))))))

