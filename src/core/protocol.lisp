(in-package #:clusivius)

(defclass stage ()
  ((id :initarg :id
       :type symbol
       :reader stage-id)
   (dispatcher :initarg :dispatcher
               :reader stage-dispatcher)
   (next-stage :initarg :next-stage
               :initform nil
               :documentation "If NIL, protocol is terminated."
               :type symbol
               :reader stage-next-stage))
  (:documentation "Represent a single protocol execution state."))

(defclass protocol ()
  ((id :initarg :id
       :type symbol
       :reader protocol-id)
   (initial-stage :initarg :initial-stage
                  :type stage
                  :reader protocol-initial-stage)
   (stages :initarg :stages
           :reader protocol-stages))
  (:documentation "Contains an info on protocol and an identifier."))

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
