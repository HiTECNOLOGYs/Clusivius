(in-package #:clusivius)

(defclass link ()
  ((resource :initarg :resource
             :initform (error "A link cannot exist without the other end.")
             :documentation "Identifies resource on the other side.
Interpretation is schema-dependant. (i.e. for HTTP it is a path to resource but
for bare TCP it might be <host>:<port>)

The implementation might choose not to use this slot on regular bases but instead
parse it once and store it using different format (or even slots). This is totally
acceptable given that it uses data EXCLUSIVELY from here as this slot is the
only place guaranteed to have correct resource locator. Also, this approach
eliminates the possibility of \"side effects\"."
             :type string
             :reader link-resource)
   (current-stage :initarg :stage
                  :type stage
                  :accessor link-current-stage)
   (initiator :initarg :initiator
              :type backend
              :documentation "Holds an instance that initiated link (e.g. either client or a server).
A correct backend MUST store it's instance there for consistency reasons."
              :reader link-initiator))
  (:documentation "Represents a link to client."))

(defclass transport ()
  ((read-callback :initarg :read-callback
                  :type function
                  :reader transport-read-callback))
  (:documentation "Defines hardware NIC bindings. This one operates on raw
bytes stream and passes them to backend to be processed. Well, actually it
knows nothing of backend and only calls whatever it's told to call.

It is up to implementation how to implement transport: the backend only sees
streams of data (either new for each data or the same, doesn't matter). This
should leave enough flexibility to implement whatever transport application may
need."))

(defmethod initialize-instance :after ((instance link) &key protocol &allow-other-keys)
  (with-slots (current-stage) instance
    (with-slots (initial-stage) protocol
      (setf current-stage initial-stage))))

(defmethod initialize-instance :before ((instance link) &key protocol &allow-other-keys)
  (with-slots (initiator) instance
    (establish-link protocol initiator instance)))

(defgeneric establish-link (protocol transport link)
  (:documentation "Opens new link.
This function is called by INITIALIZE-INSTANCE when link instance is created."))

(defgeneric close-link (protocol transport link)
  (:documentation "Terminates given link."))

(defgeneric send-message (protocol transport message)
  (:documentation "Sends encoded message to given link according to given
protocol. The message MAY be of any type but it MUST already be serialized to
bytes so that transport doesn't need to fiddle with it."))

(defgeneric receive-message (protocol transport link)
  (:documentation "Receives encoded message from given link according to given
protocol using given backend. MUST return stream with raw bytes that backend
then reads."))

(define-condition link-condition ()
  ((link :initarg :link))
  (:documentation "Connection-related conditions."))

(define-condition link-timed-out (link-condition) ()
  (:documentation "Signaled when connected has timed out."))

(define-condition link-closed (link-condition) ()
  (:documentation "Signaled when connected was closed by the other side."))

(define-condition link-terminated (link-condition) ()
  (:documentation "Signaled when connected was closed by server."))
