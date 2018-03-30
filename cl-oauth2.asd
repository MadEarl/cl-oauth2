;;; Copyright (C) 2018 Wolfgang Mederle
;;; All rights reserved.
;;; See the file LICENSE for terms of use and distribution.

(in-package #:cl-user)

(defpackage :cl-oauth2-asd
  (:use #:cl #:asdf))

(in-package :cl-oauth2-asd)

(defsystem :cl-oauth2
  :name "CL-OAuth2"
  :description "Common Lisp OAuth 2.0 implementation"
  :version "1"
  :maintainer "Wolfgang Mederle <jungleoutthere@mederle.de>"
  :licence "LLGPL"
  :components ((:static-file "cl-oauth2.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:module "util"
                                              :components ((:file "misc")
                                                           (:file "query-string"
                                                                  :depends-on ("misc"))
                                                           (:file "uri"
                                                                  :depends-on ("query-string")))
                                              :depends-on ("package"))
                                     (:module "core"
                                              :components ((:file "request-adapter")
                                                           (:file "error-handling"
                                                                  :depends-on ("request-adapter"))
                                                           (:file "parameters"
                                                                  :depends-on ("request-adapter"))
                                                           (:file "tokens")
                                                           (:file "consumer"
                                                                  :depends-on ("tokens" "parameters"
                                                                               "error-handling")))
                                              :depends-on ("package" "util")))))
  :depends-on (:ironclad :cl-base64 :babel
               :closer-mop :cl-store
               :alexandria :anaphora :f-underscore :split-sequence
               :trivial-garbage
               :drakma :cl-json
               :puri :hunchentoot))
