(use sxml.ssax)

(define (main . args)
  (let ((sxml (ssax:xml->sxml (standard-input-port) '((h . "http://www.w3.org/1999/xhtml")))))
    (write sxml))
  )
