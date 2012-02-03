;;;; test-packages.lisp
(defpackage :image-manager-test
  (:use :cl :lisp-magick :image-manager :lisp-unit))

(defpackage :auxiliary-test
  (:use :cl :auxiliary :cl-fad :lisp-unit))

(defpackage :tester-test
  (:use :cl :fann :tester :image-manager :lisp-unit))

(defpackage :segmenter-test
  (:use :cl :segmenter :lisp-magick :lisp-unit))

(defpackage :recognizer-test
  (:use :cl :recognizer :fann :lisp-unit))

