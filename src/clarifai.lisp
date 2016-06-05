(in-package #:clarifai)


(defclass session ()
  ((client-id
     :initform (error "No client ID provided")
     :initarg :client-id
     :accessor client-id)
   (client-secret
     :initform (error "No client secret provided")
     :initarg :client-secret
     :accessor client-secret)
   (access-token
     :initform nil
     :initarg :access-token
     :accessor access-token)
   (access-token-expire-time
     :initform nil
     :initarg :access-token-expire-time
     :accessor access-token-expire-time)))


(defgeneric gen-access-token (this))
(defgeneric get-access-token (this))
(defgeneric get-tags (this image-url))


(defmethod gen-access-token ((this session))
  (multiple-promise-bind
      (body status headers)
      (request "https://api.clarifai.com/v1/token"
               :method :post
               :headers '(:content-type "application/x-www-form-urlencoded")
               :body (url-encode-params `(("client_id" . ,(client-id this))
                                          ("client_secret" . ,(client-secret this))
                                          ("grant_type" . "client_credentials")))
               :return-body t)
      (declare (ignore headers))
      (if (= status 200)
        (let* ((decoded-body (decode-json-from-string (octets-to-string body)))
               (token (oauth::get-alist-value :access--token decoded-body))
               (expires-in (oauth::get-alist-value :expires--in decoded-body)))
          (setf (access-token this) token)
          (setf (access-token-expire-time this)
                (1- (+ expires-in (oauth::get-unix-time))))
          (format t "access-token = ~A, expires-in = ~A~%" token expires-in)
          token)
        (let ((decoded-body (decode-json-from-string (octets-to-string body))))
          (format t "failed to generate access token: ~A~%" `(,status ,decoded-body))
          (error (format nil "failed to generate access token: ~A~%" `(,status ,decoded-body)))))))


(defmethod get-access-token ((this session))
  (cond
    ((not (access-token this))
     (gen-access-token this))
    ((> (oauth::get-unix-time) (access-token-expire-time this))
     (gen-access-token this))
    (t
     (access-token this))))


(defmethod get-tags ((this session) image-url)
  (alet ((token (get-access-token this)))
    (multiple-promise-bind
        (body status headers)
        (request "https://api.clarifai.com/v1/tag"
                 :method :post
                 :headers `(:content-type "application/x-www-form-urlencoded"
                            :authorization ,(format nil "Bearer ~A" token))
                 :body (url-encode-params `(("url" . ,image-url)))
                 :return-body t)
      (declare (ignore headers))
      (if (= status 200)
        (let* ((decoded-body (decode-json-from-string (octets-to-string body)))
               (status-code (oauth::get-alist-value :status--code decoded-body)))
          (if (string/= status-code "OK")
            (progn
              (format t "Failed to get image tags: ~A~%" (oauth::get-alist-value :status--msg decoded-body))
              nil)
            (let* ((results (oauth/util:get-alist-value :results decoded-body))
                   (tag-list (oauth::get-deep-alist-value (nth 0 results) :result :tag :classes)))
              tag-list)))
        (progn
          (format t "Failed to get image tags: ~A~%" (octets-to-string body))
          nil)))))
