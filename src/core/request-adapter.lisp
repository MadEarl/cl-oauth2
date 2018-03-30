(in-package :oauth2)

;;; 
;;; Hunchentoot serves as callback receiver.
;;; 

(defparameter *web-server* nil)

(defun start-callback-server (token &key ssl-certificate-file ssl-privatekey-file)
  (when *web-server*
    (hunchentoot:stop *web-server*)
    (setf *web-server* nil))
  (setf *web-server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-ssl-acceptor
                        :port (token-redirect-port token)
                        :ssl-certificate-file ssl-certificate-file
                        :ssl-privatekey-file ssl-privatekey-file))))

(defun make-oauth2-callback-handler (token)
  (hunchentoot:define-easy-handler (oauth-handler :uri "/oauth" :default-request-type :both)
      (state code) ;user_id?
    (setf (hunchentoot:content-type*) "text/plain")
    (when (and state
               (not (equal state (token-state token))))
      (error "State does not match."))
    (let ((request-type (hunchentoot:request-method hunchentoot:*request*)))
      (cond (code ; then it's a response for a code request
             (setf (token-code token) code)
             (format nil "Received code ~A for scope ~A~%" code (token-scope token)))
            ((eq request-type :post) ; tokens come in json body
             (let* ((data-string (hunchentoot:raw-post-data :force-text t)))
               (format t "~A" data-string)))))))
