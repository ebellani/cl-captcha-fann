;;;; segmenter.lisp

;;;; This package is responsible for segmenting a captcha into its characters.

(in-package :segmenter)

(defclass recognized-segment ()
  ( ;; a string representing what a neural net beliefs this is. Ex: "N"
   (label  :initarg :label  :accessor label)
   ;; the degree of confidence the net has in the label. Ex: 0.8 -> 80%
   (belief :initarg :belief :accessor belief))
  (:documentation
   "Representation a single image segment, and what a neural network
  beliefs it is."))

(defmethod print-object ((object recognized-segment) stream)
  (print-unreadable-object (object stream :type t)
    (if (and (slot-boundp object 'label)
             (slot-boundp object 'belief))
        (with-slots (label belief) object
          (format stream "I believe this is ~a with ~d of certanty"
                  label belief))
        (format stream "This segment was not recognized."))))

;; TODO, accept a wand
(defun image->recognized-segments (image-wand
                                   step-%
                                   width-%
                                   recognize-function)
  "Takes an image found on IMAGE-WAND, and sweeps the image every
  step, given by the percentage of the image taken from STEP-%. The
  size of the sweep window is another percentage given by
  WIDTH-%. Recognize every segment with the RECOGNIZE-FUNCTION to
  produce a list of recognized-segments"

  (defun raw-data->recognized-segment (raw-data)
    "Uses the outer function RECOGNIZE-FUNCTION to produce the
     recognized-segment from the RAW-DATA."
    (let* ((recognizer-result  (funcall recognize-function raw-data))
           (max-belief         (reduce  #'max recognizer-result)))
      (make-instance 'recognized-segment
                     :label  (bit-list->label recognizer-result max-belief)
                     :belief max-belief)))
  (let* ((root-image-width  (magick-get-image-width  image-wand))
         (root-image-height (magick-get-image-height image-wand))
         (step-size          (floor (* root-image-width step-%)))
         (sweep-window-width (floor (* root-image-width width-%)))
         (step-limit         (- root-image-width sweep-window-width)))
    (defun sweep-accumulator (current-step-location acc-list)
      (if (>= current-step-location step-limit)
          acc-list
          (sweep-accumulator
           (+ current-step-location step-size)
           (let ((cloned-root (clone-magick-wand image-wand)))
             (magick-crop-image cloned-root
                                sweep-window-width
                                root-image-height
                                current-step-location
                                0)
             (cons (raw-data->recognized-segment
                    (get-wand-raw-data! cloned-root :float-list))
                   acc-list)))))
    (reverse (sweep-accumulator 0 ()))))



