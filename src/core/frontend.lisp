(in-package #:clusivius)

;; Currently, the only frontend is hard-coded. This is not only a subject to
;; change, but it will change some time soon.

(defun build-protocol-stage (stage-class &rest initargs)
  (apply #'make-instance stage-class initargs))

(defun parse-protocol-body (body)
  (loop for stage in body collect
        (destructuring-bind ((stage-class id next-stage) &rest initargs) stage
          (apply #'build-protocol-stage stage-class
                 :id id
                 :next-stage next-stage
                 initargs))))

(defmacro define-protocol (name (&key (protocol-class 'protocol))
                           &body body)
  (let ((stages (parse-protocol-body body)))
    `(defclass ,name (,protocol-class)
       ((initial-stage :initform ,(first stages))
        (stages :initform ,stages)))))
