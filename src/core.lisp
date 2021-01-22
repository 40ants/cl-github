(defpackage #:github/core
  (:nicknames #:github)
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:link-header
                #:with-links)
  
  (:shadow #:get)
  (:export #:get
           #:*token*
           #:*user-agent*
           #:*debug*
           #:post
           #:*base-url*
           #:*default-timeout*))
(in-package github/core)


(defvar *debug* nil)
(defvar *github-ratelimit-remaining*)
(defvar *api-hits* 0)
(defvar *token* nil)
(defvar *user-agent* "cl-github (https://github.com/40ants/cl-github)")
(defvar *base-url* "https://api.github.com")
(defvar *default-timeout* 10)


(defvar *warned-about-token* nil)


(defun int-header (headers name)
  (let ((value (gethash name headers)))
    (when value
      (parse-integer value))))


(defun sleep-and-retry-if-rate-limited (cond)
  "Respect GitHub's rate limits and stop after we hit the limit.
  
   https://developer.github.com/v3/#abuse-rate-limits
   https://developer.github.com/guides/best-practices-for-integrators/#dealing-with-abuse-rate-limits
  "
  (let* ((headers (dex:response-headers cond))
         (ratelimit-reset (int-header headers "x-ratelimit-reset"))
         (now (local-time:timestamp-to-unix (local-time:now))))
    (when ratelimit-reset
      (let ((seconds-to-sleep (- ratelimit-reset now)))
        ;; (princ (alexandria:hash-table-alist headers))
        ;; (break)
        (when (> seconds-to-sleep 0)
          (log:warn "Ratelimited, will sleep ~A seconds up to ~A~%"
                    seconds-to-sleep
                    (local-time:format-timestring
                     nil
                     (local-time:unix-to-timestamp ratelimit-reset)
                     :format local-time:+asctime-format+))
          (sleep seconds-to-sleep))
        (dex:retry-request cond)))))


(defun check-for-token ()
  (when (and (not *warned-about-token*)
             (null *token*))
    (setf *warned-about-token* t)
    (log:warn "Please, set github:*token* variable to OAuth token. This way you'll have large rate limit.")))


(defun make-headers (user-headers)
  (append
   (when *token*
     (list (cons "Authorization"
                 (concatenate 'string
                              "token "
                              *token*))))
   (list (cons "User-Agent" *user-agent*))
   user-headers))


(defun track-rate-limit (headers)
  (setf *github-ratelimit-remaining*
        (int-header headers "x-ratelimit-remaining"))
                
  (if *debug*
      (log:info "GitHub's ratelimit remaining is ~A~%"
                *github-ratelimit-remaining*)
        
      (when (and *github-ratelimit-remaining*
                 (< *github-ratelimit-remaining* 100))
        (log:warn "GitHub's ratelimit remaining is ~A~%"
                  *github-ratelimit-remaining*))))


(defun get (path &key params items verbose limit headers (timeout *default-timeout*))
  (log:debug "Fetching data from ~A with params ~A" path params)

  (check-for-token)

  (setf *api-hits* (1+ *api-hits*))
  
  (let* ((full-path (apply #'format (append (list nil path) params)))
         (url (if (starts-with-subseq "/" full-path)
                  ;; use either relative uri
                  (concatenate 'string *base-url* full-path)
                  ;; or full url with schema
                  full-path))
         (user-headers headers)
         (headers (make-headers user-headers)))
    
    (with-links (response status-code headers next-link)
                (handler-bind
                    ((dex:http-request-forbidden #'sleep-and-retry-if-rate-limited)
                     (dex:http-request-not-found #'dex:ignore-and-continue))
                  (dex:get url :headers headers
                               :verbose verbose
                               :connect-timeout timeout
                               :read-timeout timeout))

                (track-rate-limit headers)
                
                ;; we ignore 404 error and just return nil as if
                ;; no data were fetched
                (when (not (= status-code 404))
                  (let* ((decoded (jonathan:parse response))
                         (result (cond
                                   ;; This is how /search/whatever returns
                                   ;; its results
                                   ((and (listp decoded)
                                         (keywordp (car decoded))
                                         (member :|items| decoded)
                                         (member :|total_count| decoded))
                                    (append (getf decoded :|items|)
                                            items))
                                   ;; Usual list of items or a single item
                                   ((listp decoded)
                                    (append decoded items))
                                   ;; All other possible formats
                                   (t decoded))))
        
                    (if (and next-link (or (null limit)
                                           (< (length result)
                                              limit)))
                        ;; iterate over pages
                        (get next-link :items result :headers user-headers)
                        ;; or end recursion
                        result))))))


(defun post (path data &key headers verbose (timeout *default-timeout*))
  (log:debug "Posting data to" path)

  (check-for-token)

  (setf *api-hits* (1+ *api-hits*))
  
  (let* ((url (if (starts-with-subseq "/" path)
                  ;; use either relative uri
                  (concatenate 'string *base-url* path)
                  ;; or full url with schema
                  path))
         (content (jonathan:to-json data))
         (user-headers headers)
         (headers (make-headers user-headers)))
    
    (multiple-value-bind (response status-code headers)
        (handler-bind
            ((dex:http-request-forbidden #'sleep-and-retry-if-rate-limited))
          (dex:post url :content content
                        :headers headers
                        :verbose verbose
                        :connect-timeout timeout
                        :read-timeout timeout))
      (when (>= status-code 400)
        (log:error "Github reponded with" status-code))

      (track-rate-limit headers)
      
      (jonathan:parse response))))
