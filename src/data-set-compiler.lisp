;;;; data-set-compiler.lisp

;;;; used to generate a FANN dataset and other things involved in the
;;;; production and training of a NN. Usually everything in this
;;;; package will be used prior to the actual training and testing.

(in-package :data-set-compiler)

(defun compile-captcha-data-set (data-set-path
                                 destination-path
                                 &optional
                                 (header-function
                                  (lambda (number-of-training-pairs
                                           inputs
                                           outputs)
                                    (format nil "~a ~a ~a~%"
                                            number-of-training-pairs
                                            inputs outputs)))
                                 (separator #\linefeed)
                                 (image-width 16)
                                 (image-heigth 16))
  "Transverses the DATA-SET-PATH taking each image from the subdirs
and saving them into DESTINATION-PATH in a format that can be consumed
by the a particular ann. The HEADER-FUNCTION is used to generate a
header for the dataset. The default one generates a header for the
FANN system. The separator is used to separate the input from the
output in the training pair."
  (let* ((data-files       nil)
         (directory-bit-string-ht
          (get-bit-string-directories-hash data-set-path)))
    (walk-only-valid-directories data-set-path
                                 (lambda (pathname)
                                   (push pathname data-files)))
    (with-open-file (ann-data-set destination-path
                                  :direction :output
                                  :if-exists :supersede)
      (format ann-data-set
              (funcall header-function 
                       (length data-files)
                       (* image-width image-heigth)
                       (hash-table-count directory-bit-string-ht)))
      (mapc (lambda (file-pathname)
              (format ann-data-set "~a~a~a~%"
                      (with-magick-wand
                          (data-wand :load (namestring file-pathname)) 
                        (get-wand-raw-data! data-wand))
                      separator
                      (gethash (last-directory-name file-pathname)
                               directory-bit-string-ht)))
            data-files))))

(defun generate-character-images (root-pathname how-many font
                                  &optional
                                  (case-sensitive? nil)
                                  (width  32)
                                  (height 32)
                                  (chars "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  "Generates HOW-MANY slightly different CHARS of the given FONT, saving them in a directory of the same name of the char in the ROOT-PATHNAME"
  (let ((char-list (coerce chars 'list)))
    (dolist (the-char char-list)
      (let* ((string-char (string the-char))
             (char-directory
              (namestring
               (make-pathname
                :directory `(:absolute ,root-pathname
                                       ,(if case-sensitive?
                                            string-char
                                            (string-downcase string-char)))))))
        (ensure-directories-exist char-directory)
        (dotimes (i how-many)
          (with-magick-wand (wand :create width height :comp (255 255 255))
            (generate-letter wand string-char font width height)
            (pre-process-wand! wand) ;; slightly change the image
            (save-with-random-name wand char-directory)))))))

(defun generate-dataset-images (root-pathname many)
  "Takes a single image from each label directory in ROOT-PATHNAME and
  generates MANY images that are a random variation from that image
  and saves them in the same label directory."
  (walk-only-valid-directories
   root-pathname
   (lambda (data-pathname)
     (let ((data-directory (directory-namestring data-pathname)))
       (dotimes (i many)
         (with-magick-wand (data-wand :load (namestring data-pathname))
           (pre-process-wand! data-wand)
           (save-with-random-name data-wand data-directory)))))))

(defun save-with-random-name (wand dir)
  "saves WAND as a jpg with a random name in the DIR."
  (magick-write-image wand
                      (format nil "~a~a.jpg" dir
                              (write-to-string (random 99999999999999)))))

(defun move-imgs (root)
  "Moves all images from directories that are upper cased chars."
  (walk-only-valid-directories
   root
   (lambda (data-pathname)
     (let* ((data-dir-fullpath (directory-namestring data-pathname))
            (data-dir (last-directory-name data-dir-fullpath)))
       (when (upper-case-p (char data-dir 0))
         (rename-file data-pathname
                      (namestring  (make-pathname
                                    :directory
                                    `(:relative :up ,(string-downcase data-dir))
                                    :name (pathname-name data-pathname)
                                    :type (pathname-type data-pathname)))))))))

(defun convert-dataset-images (root-pathname)
  "Applies the trim and other transformations using the command line
  'convert' app to each file found on each subdir of ROOT-PATHNAME."
  (walk-only-valid-directories
   root-pathname
   (lambda (data-pathname)
     (let ((command-string (format nil "convert ~a -liquid-rescale 16x16! ~a"
                                   data-pathname
                                   data-pathname)))
       (print command-string)
       (shell-command command-string)))))

(defun split-data-set (dataset-root-path
                       destination-path
                       number-of-extractions)
  "Given a DATASET-ROOT-PATH moves to DESTINATION-PATH a
NUMBER-OF-EXTRACTIONS from each class of the original dataset path"
  (dolist (dir-pathname (list-directory dataset-root-path))
    (let ((list-of-files (list-directory dir-pathname))
          (data-dir (last-directory-name dir-pathname)))
      (shell-command (format nil "mkdir -p ~a"
                             (namestring
                              (make-pathname
                               :directory
                               `(:absolute ,destination-path
                                           ,data-dir)))))
      (dotimes (i number-of-extractions)
        (let ((data-pathname (nth i list-of-files)))
          (shell-command
           (format nil "mv ~a ~a"
                   data-pathname
                   (namestring (make-pathname
                                :directory
                                `(:absolute ,destination-path ,data-dir)
                                :name (pathname-name data-pathname)
                                :type (pathname-type data-pathname))))))))))

;; (walk-only-valid-directories
;;  "/home/b-man/Projects/captcha-data/letras_cortadas_am/"
;;  (lambda (pname)
;;    (when (< 999999 (parse-integer (pathname-name pname)))
;;      (delete-file pname))))



