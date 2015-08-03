(in-package #:clusivius)

(defclass session ()
  ((link :initarg :link
         :type link
         :accessor session-link))
  (:documentation "Holds connection session.
The backend implementation is free to choose how (and even whether) to store
this session. It MAY exist only until connection is processed or it may
persist."))

(defclass message ()
  ((session :initarg :session
            :type session
            :reader message-session)
   (content :initarg :content
            :initform nil
            :type list
            :reader message-content))
  (:documentation "Protocol message.
This stores raw or decoded message data since messages are serialized using
given backend before being sent over the wire or after being received from.

It's up to backend how to (de)serialize them. The protocol code only sees
unencoded messages (which are instances of this class or it's children)."))

(defclass backend ()
  ((protocol :initarg :protocol
             :type protocol
             :reader backend-protocol))
  (:documentation "Implements data decoding and processing.

This hooks up into transport and receives messages data from the stream,
decoding them to be processed by protocol stages. It operates below protocol
level so it's unaware of what happens to data afterwards. It's only
responsibilty is to construct valid messages and pass them on to protocol stage
handler."))

(defmethod initialize-instance :after ((instance backend) &rest initargs)
  (declare (ignore initargs))
  (init-backend instance))

(defgeneric encode-message (protocol backend message)
  (:documentation "Encodes about-to-be-sent message according to given protocol
using given backend."))

(defgeneric read-message (protocol backend stream)
  (:documentation "Read received message from stream according to given
protocol using given backend. It's done in this fasion to eliminate need for
two passages (i.e. one reads the second decodes). Hence the message is
constructed on the go and as soon as all the data is available the instance is
created and returned. It might seem like too much for one function but it's
really not as long as the reading is done using external facility that only
returnes decoded data. You could actually even map over it!"))

(defgeneric init-backend (backend)
  (:documentation "Initializes backend.
This function is called by INITIALIZE-INSTANCE when backend instance is created."))

(defgeneric deinit-backend (backend)
  (:documentation "Deinitializes backend."))

(defgeneric perform-stage (protocol stage session message)
  (:documentation "Executes protocol stage on given session."))

(defun get-stage (protocol stage-id)
  (find stage-id (protocol-stages protocol)
        :key #'stage-id))

(defmethod perform-stage ((protocol protocol)
                          (stage stage)
                          (session session)
                          message)
  (with-slots (dispatcher) stage
    (funcall dispatcher session message)))

(defmethod perform-stage :after ((protocol protocol)
                                 (stage stage)
                                 (session session)
                                 message)
  (with-slots (next-stage) stage
    (if (null next-stage)
      (signal 'protocol-terminated)
      (setf (link-current-stage (session-link session))
            (get-stage protocol next-stage)))))

(define-condition session-condition ()
  ((session :initarg :session))
  (:documentation "Session-related condition."))

(define-condition server-condition ()
  ((server :initarg :server))
  (:documentation "Server-related conditions."))

(define-condition client-condition ()
  ((client :initarg :client))
  (:documentation "Client-related conditions."))

(define-condition message-out-of-order (link-condition protocol-condition) ()
  (:documentation "Signaled when client send proper but unexpected message."))

(define-condition message-invalid (link-condition protocol-condition) ()
  (:documentation "Signaled when client send improper."))

(define-condition network-is-unreachable (client-condition) ()
  (:documentation "Signaled when client could not reach a network."))

;;; **************************************************************************
;;;  Servers
;;; **************************************************************************

(defclass server (backend) ()
  (:documentation "Implements server backend."))

(defgeneric start-server (backend)
  (:documentation "Starts protocol server with given backend."))

(defgeneric listen-for-connections (server)
  (:documentation "Blocks and listens for incoming connections.
The code on the inside should receive PROTOCOL-TERMINATED signal when it's
about to be terminated so it can do proper cleanup. That dosn't elimiate need
for UNWIND-PROTECT, though, which is recommended way to handle listening
stop."))

;;; **************************************************************************
;;;  Clients
;;; **************************************************************************

(defclass client (backend) ()
  (:documentation "Implements server backend."))

(defgeneric start-client (backend)
  (:documentation "Starts protocol client with given backend."))

(defgeneric connect-client (client uri)
  (:documentation "Connects client to given URI returning established connection.
The code on the inside should receive PROTOCOL-TERMINATED signal when it's
about to be terminated so it can do proper cleanup. That dosn't elimiate need
for UNWIND-PROTECT, though, which is recommended way to handle listening
stop."))
