(in-package #:clusivius.backend.http)

(defclass http-resource (clusivius:stage)
  ((method :initarg :method
           :reader http-resource-method)
   (uri :initarg :uri
        :reader http-resource-uri))
  (:documentation "HTTP request stage."))

(defclass http-request (clusivius:message)
  ((type :initarg :type
         :initform :get
         :reader request-type)
   (resource :initarg :resource
             :reader request-resource)
   (headers :initarg :headers
            :reader request-headers)
   (uri-parameters :initarg :uri-parameters
                   :reader request-uri-parameters)
   (post-parameters :initarg :post-parameters
                    :reader request-post-parameters))
  (:documentation "HTTP request."))

(defclass http-response (clusivius:message)
  ((type :initarg :type
         :initform :get
         :reader response-type)
   (resource :initarg :resource
             :reader response-resource)
   (headers :initarg :headers
            :reader response-headers)
   (uri-parameters :initarg :uri-parameters
                   :reader response-uri-parameters)
   (post-parameters :initarg :post-parameters
                    :reader response-post-parameters)
   (body :initarg :body
         :reader response-body))
  (:documentation "HTTP response."))

(defclass http-session (clusivius:session)
  ((data :initarg :data
         :initform (make-hash-table)
         :accessor http-session-data))
  (:documentation "Stores client data between requests."))

(defclass http-server (clusivius:server)
  ((port :initarg :port
         :initform 8080
         :reader http-server-port)
   (sessions :initarg :sessions
             :initform (make-hash-table)
             :accessor http-server-sessions)
   (dispatch-table :initarg :dispatch-table
                   :initform (make-hash-table :test 'equal)
                   :accessor http-server-dispatch-table)
   (acceptor :initarg :acceptor
             :reader http-server-acceptor))
  (:documentation "Wrapper over hunchentoot."))

(defun get-resource (server type uri)
  (gethash (list type uri) (http-server-dispatch-table server)))

(defun (setf get-resource) (new-value server type uri)
  (setf (gethash (list type uri) (http-server-dispatch-table server)) new-value))

(defun get-session ()
  (let ((session (hunchentoot:start-session)))
    (or (hunchentoot:session-value :object session)
        (setf (hunchentoot:session-value :object session)
              (make-instance 'http-session)))))

(defun parse-request (request)
  (with-slots ((headers-in hunchentoot:headers-in)
               (get-parameters hunchentoot:get-parameters)
               (post-parameters hunchentoot:post-parameters))
      request
    (make-instance 'http-request
                   :type (hunchentoot:request-method request)
                   :resource (hunchentoot:request-uri request)
                   :headers headers-in
                   :uri-parameters get-parameters
                   :post-parameters post-parameters)))

(defun initialize-dispatch-table (server stages)
  (dolist (stage stages (values))
    (setf (get-resource server
                        (http-resource-method stage)
                        (http-resource-uri stage))
          stage)))

(defclass http-acceptor (hunchentoot:acceptor)
  ((server :initarg :server
           :reader http-acceptor-server))
  (:documentation "Custom acceptor that operates on our data structures."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor http-acceptor) request)
  (let* ((server (http-acceptor-server acceptor))
         (protocol (clusivius:backend-protocol server))
         (method (hunchentoot:request-method request))
         (uri (hunchentoot:request-uri request))
         (resource (get-resource server method uri)))
    (cond
      ((null resource)
       (setf (hunchentoot:return-code hunchentoot:*reply*)
             hunchentoot:+http-not-found+)
       (hunchentoot:abort-request-handler))
      (t
       (clusivius:perform-stage protocol
                                resource
                                (get-session)
                                (parse-request request))))))

(defmethod clusivius:init-backend ((server http-server))
  (initialize-dispatch-table server
                             (clusivius:protocol-stages
                               (clusivius:backend-protocol server)))
  (setf (slot-value server 'acceptor)
        (make-instance 'http-acceptor
                       :server server
                       :port (http-server-port server))))

(defmethod clusivius:start-server ((server http-server))
  (hunchentoot:start (http-server-acceptor server))
  server)
