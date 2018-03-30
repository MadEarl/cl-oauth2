(in-package :cl-oauth2)

(setf drakma:*text-content-types*
      (cons '("application" . "json")
            drakma:*text-content-types*))

(defun store-access-token (token)
  "Persists token for later reuse."
  (cl-store:store token (token-store-path token)))

(defun obtain-grant-code (token)
  (let ((auth-uri (make-code-request-uri
                   (token-code-uri token)
                   (token-scope token)
                   (token-state token)
                   (token-client-id token)
                   (token-redirect-uri token))))
    (format t "Please authorize the request token at this URI: ~A~%" (puri:uri auth-uri))
    (finish-output nil)
    ;; Specific to SBCL and OS X
    #+(and sbcl darwin) (sb-ext:run-program "/usr/bin/open" (list (format nil "~A" (puri:uri auth-uri))) :wait nil :output nil)
    #+(and sbcl linux) (sb-ext:run-program  "/usr/bin/xdg-open" (list (format nil "~A" (puri:uri auth-uri))) :wait nil :output nil)
    ))


(defun obtain-access-token (token &key
                                    (request-method :post)
                                    (timestamp (get-unix-time))
                                    ssl-cert ssl-key)
  "Obtains initial access-token using grant code."
  (let* ((parameters (list
                      '("grant_type" . "authorization_code")
                      `("client_id" . ,(token-client-id token))
                      `("client_secret" . ,(token-client-secret token))
                      `("code" . ,(token-code token))
                      `("redirect_uri" . ,(token-redirect-uri token)))))
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (drakma:http-request (token-token-uri token)
                      :protocol :HTTP/1.1
                      :method request-method
                      :certificate ssl-cert
                      :key ssl-key
                      :parameters parameters)
      (declare (ignorable headers uri stream must-close reason-phrase))
      (let ((json-payload (json:decode-json-from-string  body)))
        (cond ((and (eql status-code 200)
                    (equalp "bearer" (find-value :token--type json-payload)))
               (setf (token-access-key-creation-time token) timestamp)
               (when (find-key :expires--in json-payload)
                 (setf (token-access-key-expires token) (find-value :expires--in json-payload)))
               (when (find-key :scope json-payload)
                 (setf (token-scope token) (find-value :scope json-payload)))
               (when (find-key :access--token json-payload)
                 (setf (token-access-key token) (find-value :access--token json-payload)))
               (when (find-key :refresh--token json-payload)
                 (setf (token-refresh-key token) (find-value :refresh--token json-payload)))
               (when (find-key :user--id json-payload)
                 (setf (token-user-id token) (find-value :user--id json-payload))))
              (t (error "Token of wrong type or error in response: ~A~%~A" status-code body)))))))
  
(defun refresh-access-token (token &key
                                     (request-method :post)
                                     (timestamp (get-unix-time))
                                    ssl-cert ssl-key)
  "Obtains fresh access-token using refresh-token."
  (let* ((parameters (list
                      '("grant_type" . "refresh_token")
                      `("client_id" . ,(token-client-id token))
                      `("client_secret" . ,(token-client-secret token))
                      `("code" . ,(token-code token))
                      `("refresh_token" . ,(token-refresh-key token)))))
    (format t "~A" parameters)
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (drakma:http-request (token-token-uri token)
                      :protocol :HTTP/1.1
                      :method request-method
                      :certificate ssl-cert
                      :key ssl-key
                      :parameters parameters)
      (declare (ignorable headers uri stream must-close reason-phrase))
      (let ((json-payload (json:decode-json-from-string body)))
        (cond ((and (eql status-code 200)
                    (equalp "bearer" (find-value :token--type json-payload)))
               (setf (token-access-key-creation-time token) timestamp)
               (when (find-key :expires--in json-payload)
                 (setf (token-access-key-expires token) (find-value :expires--in json-payload)))
               (when (find-key :scope json-payload)
                 (setf (token-scope token) (find-value :scope json-payload)))
               (when (find-key :access--token json-payload)
                 (setf (token-access-key token) (find-value :access--token json-payload)))
               (when (find-key :refresh--token json-payload)
                 (setf (token-refresh-key token) (find-value :refresh--token json-payload)))
               (when (find-key :user--id json-payload)
                 (setf (token-user-id token) (find-value :user--id json-payload))))
              (t (error "Token of wrong type or error in response.")))))))


(defun make-code-request-uri (uri scope state client-id redirect-uri &key user-parameters)
  "Return the service provider's authorization URI. Use the resulting PURI
for a redirect. [6.2.1] in 1.0." 
  (let* ((parameters (append user-parameters
                             (list (cons "response_type" "code"))
                             (list (cons "scope" (url-encode scope)))
                             (list (cons "state" (url-encode state)))
                             (list (cons "client_id" (url-encode client-id)))
                             (list (cons "redirect_uri" (url-encode redirect-uri)))))
         (puri (puri:copy-uri (puri:parse-uri uri))))
    (setf (puri:uri-query puri)
          (if (puri:uri-query puri)
              (concatenate 'string
                           (puri:uri-query puri)
                           (alist->query-string parameters))
              (alist->query-string parameters :include-leading-ampersand nil)))
    puri))

(defun access-protected-resource (uri access-token
                                  &rest kwargs
                                  &key
                                    (timestamp (get-unix-time))
                                    (request-method :get))
  "Access the protected resource at URI using ACCESS-TOKEN."
  (let ((additional-headers (list `("Authorization" . ,(concatenate 'string "Bearer " (token-access-key access-token))))))
    (multiple-value-bind (body status-code headers)
        (drakma:http-request uri
                             :protocol :HTTP/1.1
                             :method request-method
                             :additional-headers additional-headers)
      (if (eql status-code 200)
          (values body status-code nil nil headers)
          (cond
            ((and (eql status-code 401)
                  (find-key :error (get-json body)))
             (format t "INFO: refreshing access token~%")
             (refresh-access-token access-token)
             (access-protected-resource uri access-token))
            (t
             (values body status-code headers)))))))

