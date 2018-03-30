
(defmacro without-package-variance-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
       ,@body)))

(without-package-variance-warnings
  (defpackage #:cl-oauth2
    (:nicknames #:oauth2)
    (:use #:cl #:anaphora #:f-underscore #:cl-store)
    (:import-from #:hunchentoot
                  #:create-prefix-dispatcher
                  #:*dispatch-table*)
    (:import-from #:alexandria #:with-unique-names #:curry #:rcurry #:ensure-list #:compose)
    (:import-from #:split-sequence #:split-sequence)
    (:export
     #:*protocol-version*

      ;;; error handling
      #:http-error
      #:bad-request
      #:unauthorized
      #:raise-error
      #:default-error-handler
      #:protocol-assert

      #:token
      #:make-token
      #:token-access-key
      #:token-refresh-key
      #:token-code
      #:token-state
      #:token-access-key-creation-time
      #:token-redirect-uri
      #:token-user-data
      #:token-session-handle
      #:token-client-id
      #:token-client-secret
      #:token-access-key-expires
      #:token-scope
      #:token-code-uri
      #:token-token-uri
      #:token-expired-p
      #:token-user-id
      #:token-store-path

      ;;; consumer functions
      #:obtain-access-token
      #:make-code-request-uri
      #:obtain-grant-code
      #:refresh-access-token
      #:access-protected-resource
      #:store-access-token

      ;;; utility functions
      #:find-key
      #:find-value
      #:get-json
      
      ;;; parameters
      #:remove-auth-parameters
      #:normalized-parameters

      ;;; service provider
      #:check-version
      #:finalize-callback-uri
      #:make-response

      ;;; request adapter
      #:*web-server*
      #:start-callback-server
      #:make-oauth2-callback-handler
      
      )))

