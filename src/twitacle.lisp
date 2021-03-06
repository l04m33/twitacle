(in-package #:twitacle)


(defparameter *resource-base-url*  "https://api.twitter.com/1.1/")

(defparameter *tweet-count* 20)
(defparameter *refresh-interval* 20)  ; in seconds


(defun login (session)
  (alet ((resp (request-token session
                              "https://api.twitter.com/oauth/request_token"
                              "oob"
                              :method :post)))
    (if (= (nth 0 resp) 200)
      (progn 
        (format t "Authorization URL: ~A~%"
                (build-authorization-url session
                                         "https://api.twitter.com/oauth/authenticate"))
        (format t "Enter verifier: ")
        (finish-output)
        (let ((verifier (read-line)))
          (alet ((resp (access-token session
                                     "https://api.twitter.com/oauth/access_token"
                                     verifier
                                     :method :post)))
            (if (= (nth 0 resp) 200)
              t
              (progn
                (format t "access-token failed: ~A~%" (nth 1 resp))
                (error (format nil "access-token failed: ~A" (nth 1 resp))))))))
      (progn
        (format t "request-token failed: ~A~%" (nth 1 resp))
        (error (format nil "request-token failed: ~A" (nth 1 resp)))))))


(defun do-search (session params query upper-id lower-id clarifai-session)
  (format t "================================~%")
  (format t "params = ~A~%" params)
  (format t "upper-id = ~A, lower-id = ~A~%" upper-id lower-id)
  (let* ((search-request-promise
           (alet ((search-resp (request session
                                        "search/tweets.json"
                                        :method :get
                                        :params (cons `("q" . ,query) params))))
             (list session search-resp query upper-id lower-id clarifai-session params)))
         (result-handled-promise (attach search-request-promise #'handle-search-result))
         (catcher-promise (catcher result-handled-promise
                            (t (e)
                              (format t "Search operation failed: ~A~%" e)
                              (list (* 2 *refresh-interval*)
                                    session
                                    params
                                    query
                                    upper-id
                                    lower-id
                                    clarifai-session)))))
    (attach catcher-promise #'schedule-do-search)))


(defun schedule-do-search (r)
  (delay #'(lambda () (apply #'do-search (cdr r)))
         :time (car r)))


(defun analyze-tweet (tweet session clarifai-session)
  (let ((image-links (extract-image-links tweet))
        (tweet-id (oauth/util:get-alist-value :id tweet)))
    (format t "  image links: ~A~%" image-links)
    (when image-links
      (catcher (alet ((tags (clarifai:get-tags clarifai-session (nth 0 image-links))))
                 (format t "Clarifai tags for tweet ~A: ~A~%" tweet-id tags)
                 (when (find "cat" tags :test #'equal)
                   (alet ((rt-ret (request session
                                           (format nil "statuses/retweet/~A.json" tweet-id)
                                           :method :post)))
                     (if (= (nth 0 rt-ret) 200)
                       (format t "Retweeted ~A~%" tweet-id)
                       (format t "Retweet failed: ~A~%" (nth 1 rt-ret))))))
        (t (e)
          (format t "Tweet analysis failed: ~A~%" e))))))


(defun handle-search-result (ret)
  (let ((session (nth 0 ret))
        (search-resp (nth 1 ret))
        (query (nth 2 ret))
        (upper-id (nth 3 ret))
        (lower-id (nth 4 ret))
        (clarifai-session (nth 5 ret))
        (last-params (nth 6 ret)))
    (case (nth 0 search-resp)
      (200
       (let* ((resp-body (nth 1 search-resp))
              (max-id (oauth::get-deep-alist-value resp-body
                                                   :search--metadata :max--id))
              (statuses (oauth::get-deep-alist-value resp-body :statuses))
              (cur-min-id (loop for s in statuses
                                ;do (format t "tweet: ~A~%" (oauth::get-alist-value :text s))
                                minimize (oauth::get-alist-value :id s))))
         
         (format t "max-id = ~A, cur-min-id = ~A~%" max-id cur-min-id)
         (format t "total count = ~A~%" (length statuses))

         (when (> (length statuses) 0)
           (format t "--------------------------------~%")
           (loop for s in statuses
                 when (and (not (oauth/util:get-alist-value :retweeted s))
                           (not (oauth/util:get-alist-value :retweeted--status s))
                           ;(string/= (subseq (oauth/util:get-alist-value :text s) 0 2) "RT")
                           (or (> (oauth/util:get-alist-value :favorite--count s) 1)
                               (> (oauth/util:get-alist-value :retweet--count s) 1)))
                 do (format t "tweet: ~A~%" (oauth/util:get-alist-value :text s))
                    (analyze-tweet s session clarifai-session)
                 end)
           (format t "--------------------------------~%"))

         (format t "cl-async stats: ~A~%" (stats))

         (if (or (< (length statuses) *tweet-count*) (not lower-id))
           (let* ((new-upper-id (or upper-id max-id))
                  (search-params `(("since_id" . ,new-upper-id)
                                   ("count" . ,*tweet-count*)
                                   ("result_type" . "recent"))))
             (format t "next search params: ~A~%" search-params)
             (list *refresh-interval*
                   session
                   search-params
                   query
                   nil
                   new-upper-id
                   clarifai-session))
           (let ((new-upper-id (or upper-id max-id))
                 (search-params `(("since_id" . ,lower-id)
                                  ("max_id" . ,(1- cur-min-id))
                                  ("count" . ,*tweet-count*)
                                  ("result_type" . "recent"))))
             (format t "next search params: ~A~%" search-params)
             (list 0
                   session
                   search-params
                   query
                   new-upper-id
                   lower-id
                   clarifai-session)))))
      (t
        (format t "Search API call failed: ~A~%" search-resp)
        (list (* 2 *refresh-interval*)
              session
              last-params
              query
              upper-id
              lower-id
              clarifai-session)))))


(defun extract-image-links (tweet)
  (let ((media (oauth::get-deep-alist-value tweet :entities :media)))
    (loop for m in media
          when (string= (oauth::get-alist-value :type m) "photo")
          collect (oauth::get-alist-value :media--url m))))


(defun main (consumer-key
             consumer-secret
             clarifai-id
             clarifai-secret)
  (with-event-loop (:catch-app-errors t)
    (let ((session (make-session :consumer-key consumer-key
                                 :consumer-secret consumer-secret
                                 :resource-base-url *resource-base-url*))
          (clarifai-session (make-instance 'clarifai:session
                                           :client-id clarifai-id
                                           :client-secret clarifai-secret))
          (login-successful t))
      (wait (catcher (login session)
              (t (e)
                (format t "Login failed: ~A~%" e)
                (setf login-successful nil)))
        (when login-successful
          (format t "Login successful~%")
          (do-search session
                     `(("count" . ,*tweet-count*)
                       ("result_type" . "recent"))
                     "貓 OR 猫 OR 喵 filter:images"
                     nil nil clarifai-session))))))
