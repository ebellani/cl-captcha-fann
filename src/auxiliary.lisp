;;;; auxiliary.lisp

(in-package :auxiliary)

(defvar *forbidden-pathnames-patterns* '("/**/.svn/**/"))

(defmacro walk-only-valid-directories
    (root-path a-function)
  "Transverses all directories found on ROOT-PATH, except for those that are invalid, and applies A-FUNCTION to each path found."
  `(walk-directory ,root-path
                   (lambda (a-pathname)
                     (when (valid-pathname-p a-pathname)
                       (funcall ,a-function a-pathname)))))

(defun valid-pathname-p (a-pathname)
  (if (null (remove-if-not (lambda (pattern)
                             (pathname-match-p a-pathname pattern))
                           *forbidden-pathnames-patterns*))
      t nil))

(defun last-directory-name (a-pathname)
  (first (last (pathname-directory a-pathname))))


(defun directory-list->bit-string-hash (dir-list)
  "Produces a hash containing a representation of every directory in DIR-LIST. Ex:
'(a b c) => '(\"1 0 0\" \"0 1 0\" \"0 0 1\")"
  (let ((list-size (length dir-list))
        (bit-string-hash (make-hash-table :test 'equal))
        (index 0))
    (mapc (lambda (dir)
            (setf (gethash (last-directory-name dir)
                           bit-string-hash)
                  (integer->bit-string index list-size))
            (incf index))
          dir-list)
    bit-string-hash))


(defun integer->bit-string (position size)
  (defun acc-bit-string (current-size bit-list)
    (if (>= current-size size)
        (string-right-trim " " (coerce bit-list 'string))
        (acc-bit-string (1+ current-size)
                        (append bit-list
                                (list (if (= current-size position)
                                          #\1 #\0)
                                      #\space)))))
  (if (>= position size)
      nil
      (acc-bit-string 0 nil)))

(defun get-bit-string-directories-hash (root-path)
  "From the ROOT-PATH, takes every valid directory and creates hash with the directory name as a key, an a bit string indicating the position of the directory.
Ex:
with 3 directories: a b c in the root dir, this function will return
a -> (1 0 0)
b -> (0 1 0)
b -> (0 0 1)"
  (directory-list->bit-string-hash
   (remove-if-not #'valid-pathname-p
                  (list-directory root-path))))

(defun bit-list->label (bit-list &optional (max-value (reduce #'max bit-list)))
  "Takes a BIT-LIST (likely the output of an ann) and returns labels.
Assumes that the labels start from 0->9 followed by letters a-A->z-Z."
  (let ((bit-position (position max-value bit-list)))
    (aref *simple-output* bit-position)
    ;; (if bit-position
    ;;     (string (code-char (if (<= bit-position 9)
    ;;                            (+ 48 bit-position)     ; number
    ;;                            (let ((ascii-char-number (- bit-position 9)))
    ;;                              (if (> ascii-char-number 26)
    ;;                                     ; lower case
    ;;                                  (+ 70 ascii-char-number)
    ;;                                     ; upper case
    ;;                                  (+ 64 ascii-char-number))))))
    ;;     nil)
    ))

(defvar *simple-output*
  "123456789abcdefghijklmnopqrstuvwxyz"
  "All the characters allowed in normal ann output.")


