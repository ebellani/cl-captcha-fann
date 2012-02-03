;;;; auxiliary-test.lisp
(in-package :auxiliary-test)

(define-test bits-suite
  (assert-equal (integer->bit-string 3 5) "0 0 0 1 0")
  (assert-equal (integer->bit-string 4 5) "0 0 0 0 1")
  (assert-false (integer->bit-string 5 5))
  (assert-equal (integer->bit-string 0 5) "1 0 0 0 0")
  (assert-equal (bit-list->label '(0.0d0 0.0d0 0.0d0 1.0d0))
                "3")
  (assert-equal (bit-list->label
                 '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 1.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0))
                "8")
  (assert-equal (bit-list->label
                 '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   1.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))
                "A")
  (assert-equal (bit-list->label
                 '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 1.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))
                "E")
  (assert-equal (bit-list->label
                 '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 1.0d0
                   0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))
                "i"))

(run-tests bits-suite)
