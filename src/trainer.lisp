;;;; captcha.lisp

;;;; this package is used to create and train FANN nets from a created data set
(in-package :trainer)

(defvar *desired-error-rate* 1.0e-5)

(defmacro with-training-data (a-pathname &body body)
  "Unhygienic macro that leaks the symbol data that is a training data
in A-PATHNAME to be used in the BODY"
  `(let ((data (read-train-data-from-file ,a-pathname)))
     (progn
       ,@body)))

(defun cascade-train-ann-on-file (pathname &optional (max-neurons 1500))
  "Uses PATHNAME to load the data to create an ann and cascade trains
it with MAX-NEURONS"
  (let ((ann (create-neural-network '(1024 35) :type :shortcut)))
    (with-training-data pathname
      (setf (activation-function-output ann) :linear)
      (setf (train-error-function ann) :errorfunc-linear)
      (cascade-train-on-data ann data max-neurons *desired-error-rate*)
      ann)))

(defun fixed-train-ann-on-file (pathname &optional (max-epochs 50))
  "Uses PATHNAME to load the data to create an ann and trains it with
the MAX-EPOCHS"
  (let ((ann (create-neural-network '(256 128 35))))
    ;; (setf (activation-function-output ann) :sigmoid-symmetric)
    (setf (activation-function-hidden ann) :sigmoid)
    (setf (training-algorithm ann)         :train-rprop)
    ;; (randomize-weights ann -0.5d0 0.5d0)
    (with-training-data 
      pathname 
      (init-weights ann data)
      (train-on-data ann data max-epochs 0 *desired-error-rate*) ann)))

(defun train-and-save-to (data-set-filepath
                          to-filepath
                          ann-create-and-train-function)
  "Loads the data-set from DATA-SET-FILEPATH, creates the ann using
the ANN-CREATE-AND-TRAIN-FUNCTION and saves the returning ann in the
TO-FILEPATH"
  (let ((trained-ann
         (funcall ann-create-and-train-function data-set-filepath)))
    (save-ann-to-file trained-ann to-filepath)
    trained-ann))


(defun pulse-train (number-of-epochs
                    number-of-times
                    train-data
                    test-data
                    &optional
                    (save-to-filepath nil))
  "This trains a standart fann network for NUMBER-OF-EPOCHS for a
NUMBER-OF-TIMES on the TRAIN-DATA. After a single set of epochs, test
the network on the TEST-DATA. If there is a SAVE-TO-FILEPATH, writes
the network on it."
  (defun train-loop (ann current-epoch current-loop)
    (cond ((>= current-loop number-of-times) ann)
          ((>= current-epoch number-of-epochs)
           (train-loop ann 0 (add1 current-loop)))
          (t (progn
               (format *standard-output*
                       "MSE during training ~a"
                       (train-epoch ann train-data))
               (format *standard-output*
                       "MSE after training ~a"
                       (mse ann))
               (format *standard-output*
                       "MSE during testing ~a"
                       (test-on-data ann test-data))
               (train-loop ann (add1 current-epoch) current-loop)))))
  (let ((ann (create-neural-network '(256 80 62))))
    ;; (setf (activation-function-output ann) :sigmoid-symmetric)
    ;; set the training algotithm as described by
    ;; http://leenissen.dk/fann/html/files/fann_data-h.html#fann_train_enum
    (setf (activation-function-hidden ann) :sigmoid-symmetric)
    (setf (training-algorithm ann)         :train-quickprop)
    (randomize-weights ann 0.01d0 0.1d0)
    (setf ann (train-loop ann 0 0))
    (when save-to-filepath
      (save-ann-to-file save-to-filepath))
    ann))


;; usage example:
;; (time (train-and-save-to (asdf:system-relative-pathname 'captcha "data/data-set/captcha-ds.data") (asdf:system-relative-pathname 'captcha "data/networks/large-150neurons-40epochs.net") #'fixed-train-ann-on-file))
