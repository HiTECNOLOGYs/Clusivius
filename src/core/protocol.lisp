(in-package #:clusivius)

(defclass stage ()
  ((next-stage :initarg :next-stage
               :reader stage-next-stage
               :documentation "If NIL, protocol is terminated."))
  (:documentation "Represent a single protocol execution state."))

(defclass protocol ()
  ((id :initarg :id
       :reader protocol-id)
   (initial-stage :initarg :initial-stage
                  :reader protocol-initial-stage)
   (stages :initarg :stages
           :reader protocol-stages))
  (:documentation "Contains an info on protocol and an identifier."))

(defgeneric perform-stage (protocol stage)
  (:documentation "Executes protocol stage."))

(define-condition protocol-condition ()
  ((protocol :initarg :protocol))
  (:documentation "Protocol-related conditions."))

(define-condition protocol-terminated (protocol-condition) ()
  (:documentation "Signaled when protocol is terminated either explicitly or due to errors."))

(define-condition protocol-error (protocol-condition) ()
  (:documentation "Signaled when protocol failed to handle a stage."))

(define-condition protocol-warning (protocol-condition) ()
  (:documentation "Signaled when non-fated error occured during protocol stage."))

(define-condition protocol-violation (protocol-condition) ()
  (:documentation "Signaled when protocol was executed improperly."))
