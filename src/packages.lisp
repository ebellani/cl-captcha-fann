;;;; packages.lisp
(defpackage :auxiliary
  (:use :cl :cl-fad)
  (:export walk-only-valid-directories
           directory-list->bit-string-hash
           last-directory-name
           integer->bit-string
           bit-list->label
           get-bit-string-directories-hash))

(defpackage :image-manager
  (:use :cl :lisp-magick :auxiliary)
  (:export pre-process-wand!
           draw-random-line!
           clean-wand!
           deform-wand!
           get-wand-raw-data!
           generate-letter
           ))

(defpackage :data-set-compiler
  (:use :cl :image-manager :trivial-shell :cl-fad :auxiliary :lisp-magick)
  (:export directory-list->bit-string-hash
           generate-dataset-images
           integer->bit-string))

(defpackage :trainer 
  (:use :cl :fann))

(defpackage :tester
  (:use :cl :auxiliary :fann :image-manager :lisp-magick)
  (:export test-ann-on-image-tree))

(defpackage :segmenter
  (:use :cl :lisp-magick :image-manager :auxiliary)
  (:export image->recognized-segments recognized-segment label belief))

(defpackage :recognizer
  (:use :cl :cl-hmm :segmenter :fann :auxiliary)
  (:export image->segments ))

