(define-module ext.tws-client
  (export <tws-client>
          make-tws-client
          tws-client-connect
          tws-client-connected?
          tws-client-process-messages
          tws-client-historical-data-request)
  )
(select-module ext.tws-client)

;; Loads extension
;;(dynamic-load "tws-client")
