;;;; captcha.asd
(asdf:oos 'asdf:load-op :fann)


(defsystem :captcha
  :description "Library that uses FANN to make sense of CAPTCHAs."
  :author "Eduardo Bellani <ebellani@gmail.com>"
  :maintainer "Eduardo Bellani <ebellani@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:fann :lisp-unit :lisp-magick :cl-fad :cl-hmm :trivial-shell)
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "auxiliary")
                                     (:file "image-manager")
                                     (:file "recognizer")
                                     (:file "trainer")
                                     (:file "tester")
                                     (:file "segmenter")
                                     (:file "data-set-compiler")))
               (:module "tst"
                        :serial t
                        :components ((:file "test-packages")
                                     (:file "image-manager-test")
                                     (:file "tester-test")
                                     (:file "recognizer-test")
                                     (:file "segmenter-test")
                                     (:file "auxiliary-test")))))

