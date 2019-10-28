(use srfi-19)
(use util.match)

(add-load-path "./lib/")
(use trader)

(define *positions* (make-hash-table))

(define *gain-thresholds* '(0.01 0.02 0.03 0.04 0.05))
(define *total-with-thresholds* (make-hash-table))

(let loop ((total-gain 0))
  (let ((line (read)))
    (if (eof-object? line)
        (begin
          (display (hash-table->alist *total-with-thresholds*) (current-error-port))
          (newline (current-error-port)))
        (match line
               (('position _ ...)
                (let ((pos (deserialize-position line)))
                  (hash-table-put! *positions* (position-index pos) pos)
                  (loop total-gain)))
               (('close id total pos-idx-close ('close pos-idx price result gain))
                (let* ((pos (hash-table-get *positions* pos-idx))
                       (info (position-info pos)))
                  (for-each (lambda (thr)
                              (if (< (pos-info-gain info) thr)
                                  (hash-table-put! *total-with-thresholds* thr
                                                   (+ (hash-table-get *total-with-thresholds* thr 0)
                                                      gain))))
                            *gain-thresholds*)
                  (print #`",(pos-info-long-trend-error info) ,gain ,id")
                  (loop (+ total-gain gain))))
               (else (loop total-gain))))))
