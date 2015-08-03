(defsystem #:clusivius
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Networking made easy."
  :depends-on (#:alexandria ; General toolkit
               #:closer-mop ; MOP
               )
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "utilities")
               (:module "core"
                        :serial t
                        :components ((:file "protocol")
                                     (:file "transport")
                                     (:file "backend")
                                     (:file "frontend")))))

(defsystem #:clusivius.backend.http
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :depends-on (#:alexandria  ; General toolkit
               #:clusivius   ; Well, you get that?
               #:hunchentoot ; HTTP server
               #:drakma      ; HTTP client
               )
  :serial t
  :pathname "src/backends/http"
  :components ((:file "packages")
               (:file "http")))
