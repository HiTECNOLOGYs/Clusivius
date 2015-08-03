(defpackage #:clusivius.backend.http
  (:use #:cl
        #:alexandria)

  ;; Resources
  (:export #:http-resource
           #:method
           #:uri
           #:http-resource-method
           #:http-resource-uri)

  ;; Requests
  (:export #:http-request
           #:type
           #:resource
           #:headers
           #:uri-parameters
           #:post-parameters
           #:http-request-type
           #:http-request-resource
           #:http-request-headers
           #:http-request-uri-parameters
           #:http-request-post-parameters)

  ;; Requests
  (:export #:http-response
           #:type
           #:resource
           #:headers
           #:uri-parameters
           #:post-parameters
           #:http-response-type
           #:http-response-resource
           #:http-response-headers
           #:http-response-uri-parameters
           #:http-response-post-parameters)

  ;; Session
  (:export #:http-session
           #:data
           #:http-session-data)

  ;; Server
  (:export #:http-server
           #:port
           #:sessions
           #:dispatch-table
           #:http-server-port
           #:http-server-sessions
           #:http-server-dispatch-table))
