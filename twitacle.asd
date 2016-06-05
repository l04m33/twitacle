(asdf:defsystem #:twitacle
  :description "Yet another twitter bot"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-async
               #:cl-async-oauth
               #:blackbird)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "package")
                             (:file "clarifai" :depends-on ("package"))
                             (:file "twitacle" :depends-on ("package" "clarifai"))))))
