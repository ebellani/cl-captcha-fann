;;;; image-manager.lisp

;;;;  package to deal with the functions that handle images. uses a
;;;;  lot of image magick.

(in-package :image-manager)

;; distorts the image using imagemagick
;; SRT default distortion function, with random
;; values.
;; SRT API: Scale, Angle -> centered scale and rotate.
(defun scale-rotate-translate! (a-wand)
  (magick-distort-image
   a-wand
   :scale-rotate-translate
   (list (/ (+ (1+ (random 10)) 90) 100)
         (- (random 18) 7)) nil))

;; swirl from -20 to 20 degrees
(defun swirl! (a-wand)
  (magick-swirl-image a-wand (- (random 41) 20)))

(defun wave! (a-wand)
  (magick-wave-image     
   a-wand
   (- (random 11) 5)
   (magick-get-image-width a-wand)))

(defun deform-wand! (wand)
  "Performs random warping and deformations destructively on the WAND.
Randomly picks one out of a list of possible warps. Resize the image to
its original size after the warping.
For an intesting reading about using imagemagick to warp images, see
http://www.imagemagick.org/Usage/warping/
http://imagemagick.org/Usage/distorts/
"
  (defun random-recur (wand percentage)
    (if (= (random percentage) 0)
        (deform-wand! wand)
        wand))
  (let* ((deformations
          (list #'scale-rotate-translate!
                #'swirl!
                #'wave!))
         (number-of-deformations (length deformations)))
    (funcall (nth (random number-of-deformations) deformations) wand)))

(defun pre-process-wand! (wand)
  "run all the pre processing available destructively in WAND"
  (scale-rotate-translate! wand)
  ;; (let ((original-width  (magick-get-image-width wand))
  ;;       (original-height (magick-get-image-width wand)))
  ;;   ;; (magick-blur-image wand 0 1)
  ;;   (scale-rotate-translate! wand)
  ;; (magick-threshold-image wand 40000)
  ;;   ;; (deform-wand! wand)
  ;;   (magick-scale-image wand original-width original-height))
  )

(defun clean-wand! (wand width height)
  "Resize WAND to WIDTH and HEIGHT and applies all the known functions
  to clean it image as good as we can."
  (magick-scale-image wand width height)
  ;; (magick-blur-image wand 0 1)
  ;; (magick-threshold-image wand 65535)
  wand)

(defun get-wand-raw-data! (wand &optional (as :string))
  "uncompress, takes its blob, removes the first 2 lines, as they are
metadata of the pbm format, and remove everything from the
blob (including the new lines) except the 0, 1 and spaces. Use the AS
keyword to control the type of return.  AS can be 'string and
'float-list for now. If any other value is used the char list is
returned. "
  (magick-set-format wand "PGM")
  (magick-set-compression wand :none)
  (let* ((blob-char-list
          (mapcar #'code-char (coerce (magick-get-image-blob wand) 'list)))
         (cleaned-string-list
          (acc-image-string (make-string-input-stream
                             (coerce blob-char-list 'string))
                            ""
                            0))
         (float-list (floats-splitted-by cleaned-string-list)))
    (cond
      ((equal as :string)
       (mapc (lambda (x)
               (format nil "~f" x))
             float-list))
      ((equal as :float-list) float-list)
      (t blob-char-list))))

(defun acc-image-string (string-input-stream
                         acc-string
                         line-number &optional (normalize-with 1.0d0))
  (let ((line (read-line string-input-stream nil)))
    (if line
        (acc-image-string string-input-stream
                          (if (or (= line-number 0)
                                  (= line-number 1)
                                  (= line-number 2))
                              acc-string
                              (concatenate 'string acc-string line))
                          (1+ line-number))
        acc-string)))

(defun generate-letter (a-white-wand letter font width height)
  "writes a black LETTER of a given FONT kind of centralized in A-WHITE-WAND."
  (with-drawing-wand (dw)
      (with-pixel-wand (pw :comp (0 0 0))
        (draw-set-stroke-color dw pw))
      (draw-set-stroke-width dw 1d0)
      (draw-set-font-size dw (* height 3/4))
      (draw-annotation dw (/ width 4) (* height 3/4) letter)
      (magick-draw-image a-white-wand dw)))

(defun floats-splitted-by (string &optional
                           (by-char #\space)
                           (normalizator 255d0))
  "Returns a list of floats of the string divided by the given char."
  (let ((trimmed-str (string-right-trim (list by-char) string)))
    (loop for i = 0 then (1+ j)
       as j = (position by-char trimmed-str :start i)
       collect (/ (float (parse-integer (subseq trimmed-str i j)) 0d0)
                  normalizator) 
       while j)))