(defpackage #:clusivius.utilities
  (:nicknames #:janus.utilities)
  (:documentation "Generic code.")
  (:use #:closer-common-lisp
        #:alexandria))

(defpackage #:clusivius
  (:nicknames #:janus)
  (:documentation "Core components.")
  (:use #:closer-common-lisp
        #:clusivius.utilities
        #:alexandria)

  ;; Protocol
  (:export #:stage
           #:id
           #:dispatcher
           #:next-stage
           #:stage-id
           #:stage-dispatcher
           #:stage-next-stage
           #:protocol
           #:id
           #:initial-stage
           #:stages
           #:perform-stage
           #:protocol-id
           #:protocol-initial-stage
           #:protocol-stages
           #:protocol-condition
           #:protocol-terminated
           #:protocol-error
           #:protocol-warning
           #:protocol-violation)

  ;; Transport
  (:export #:link
           #:resource
           #:current-stage
           #:initiator
           #:link-resource
           #:link-current-stage
           #:link-initiator
           #:transport
           #:read-callback
           #:transport-read-callback
           #:establish-link
           #:close-link
           #:send-message
           #:receive-message
           #:link-condition
           #:link-timed-out
           #:link-closed
           #:link-terminated)

  ;; Backend
  (:export #:session
           #:link
           #:session-link
           #:message
           #:content
           #:session
           #:message-content
           #:message-session
           #:backend
           #:protocol
           #:backend-protocol
           #:encode-message
           #:read-message
           #:init-backend
           #:deinit-backend
           #:session-condition
           #:server-condition
           #:client-condition
           #:message-out-of-order
           #:message-invalid
           #:network-is-unreachable
           #:server
           #:start-server
           #:listen-for-connections
           #:client
           #:start-client
           #:connect-client))
