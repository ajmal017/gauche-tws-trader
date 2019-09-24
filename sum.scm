(use srfi-19)
(use util.match)

(add-load-path "./lib/")
(use trader)

(define *positions* (make-hash-table))

(define *thresholds* '(0.0001 0.0005 0.001 0.005))
(define *total-with-threadholds* (make-hash-table))

(let loop ((total-gain 0))
  (let ((line (read)))
    (if (eof-object? line)
        (begin
          (print (hash-table->alist *total-with-threadholds*)))
        (match line
               (('position _ ...)
                (let ((pos (deserialize-position line)))
                  (hash-table-put! *positions* (position-index pos) pos)
                  (loop total-gain)))
               (('close total ('close pos-idx price result gain))
                (let* ((pos (hash-table-get *positions* pos-idx))
                       (info (position-info pos)))
                  (for-each (lambda (thr)
                              (if (> gain thr)
                                  (hash-table-put! *total-with-threadholds* thr
                                                   (+ (hash-table-get *total-with-threadholds* thr 0)
                                                      gain))))
                            *thresholds*)
                  (print #`",(pos-info-gain info) ,gain")
                  (loop (+ total-gain gain))))
               (else (loop total-gain))))))
