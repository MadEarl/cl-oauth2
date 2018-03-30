(in-package :oauth2)

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
                   (intern (symbol-name (car i)) keyword-package)
                   (intern (string-upcase (car i)) keyword-package))
       collect (cdr i))))

(defun string-or-octets->octets (x)
  (etypecase x
    (string (babel:string-to-octets x))
    ((simple-array (unsigned-byte 8)) x)))

(defun splice-alist (alist)
  (reduce #'nconc (mapcar (lambda (x)
                            (list (car x) (cdr x)))
                          alist)))

(defun between (what lower upper)
  (and (>= what lower) (<= what upper)))

(defconstant +unix-to-universal-time+ 2208988800)

(defun get-unix-time (&optional (ut (get-universal-time)))
  (- ut +unix-to-universal-time+))

(defmacro find-key (key decoded-json)
  `(find ,key ,decoded-json :key #'car :test #'equal))

(defun get-json (body)
  "Turns octets into cl-json object."
  (json:decode-json-from-string body))

(defun find-value (key decoded-json)
  (cdr (find-key key decoded-json)))
  
(defun generate-random-string (&optional (size 5))
  (with-open-file (in "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
    (with-output-to-string (out)
      (loop :repeat size
         :do (write (read-byte in) :stream out :pretty nil :base 36)))))

