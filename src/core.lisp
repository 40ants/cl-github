(defpackage #:github/core
  (:nicknames #:github)
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:secret-values)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:link-header
                #:with-links)
  (:import-from #:rutils
                #:plistp
                #:alistp)
  
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
   (when (secret-values:ensure-value-revealed *token*)
     (list (cons "Authorization"
                 (concatenate 'string
                              "token "
                              (secret-values:ensure-value-revealed *token*)))))
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


(defun get (path &key params
                   items
                   verbose
                   limit
                   headers
                   (timeout *default-timeout*)
                   (dont-warn nil)
                   (if-not-found :ignore))
  "Fetches data from the GitHub.

   Params should be a list of argument which will be applied to the path like:

   (format nil path param1 param2 ...)

   When IF-NOT-FOUND is :IGNORE, then returns NIL. If :ERROR, then DEX:HTTP-REQUEST-NOT-FOUND
   will be signalled.
"
  (check-type if-not-found (member :ignore :error))
  (check-for-token)
  
  (when (and params
             (null dont-warn)) 
    (when (alistp params)
      (log:warn "You passed alist as PARAMS. Probably you misused github:get. To suppress this warning pass :dont-warn t."))
    (when (plistp params)
      (log:warn "You passed plist as PARAMS. Probably you misused github:get. To suppress this warning pass :dont-warn t.")))

  (incf *api-hits*)
  
  (let* ((full-path (apply #'format (append (list nil path) params)))
         (url (if (starts-with-subseq "/" full-path)
                  ;; use either relative uri
                  (concatenate 'string *base-url* full-path)
                  ;; or full url with schema
                  full-path))
         (user-headers headers)
         (headers (make-headers user-headers)))

    (log:warn "Fetching data from ~A" url)
    
    (with-links (response status-code headers next-link)
                (handler-bind
                    ((dex:http-request-forbidden #'sleep-and-retry-if-rate-limited)
                     (dex:http-request-not-found (lambda (error)
                                                   (declare (ignore error))
                                                   ;; Return NIL on 404
                                                   (when (eql if-not-found :ignore)
                                                     (return-from get nil)))))
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
                                         (member :|total_count| decoded))
                                    ;; Some urls return response with two keys:
                                    ;; "total-count" and "items"
                                    ;; or
                                    ;; "total-count" and "workflow_runs"
                                    ;; thus we need to figure out the key
                                    ;; containing a list of items:
                                    (let* ((items-keys
                                             (loop for key in decoded by #'cddr
                                                   unless (eql key :|total_count|)
                                                     collect key))
                                           (key (first items-keys)))
                                      (unless (= (length items-keys)
                                                 1)
                                        (error "Number of keys in reponse should be 1. Found keys: ~A"
                                               items-keys))

                                      (setf
                                       (getf items key)
                                       (append (getf items key)
                                               (getf decoded key))
                                       ;; Also need to keep total count:
                                       (getf items :|total_count|)
                                       (getf decoded :|total_count|))
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

  (incf *api-hits*)
  
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
