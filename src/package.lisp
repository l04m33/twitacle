(in-package #:cl-user)


(defpackage #:twitacle
  (:use #:cl #:cl-async #:cl-async-oauth #:blackbird)
  (:export #:main))


(defpackage #:clarifai
  (:use #:cl
        #:carrier
        #:cl-async
        #:blackbird
        #:quri
        #:cl-json
        #:babel)

  (:export #:session

           #:client-id
           #:client-secret
           #:access-token
           #:access-token-expire-time

           #:gen-access-token
           #:get-access-token
           #:get-tags))
