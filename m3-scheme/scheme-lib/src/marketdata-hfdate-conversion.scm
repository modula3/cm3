;; $Id$

(define (date-from-index tz-name)
  (lambda (market-data index)
    ;; get date from an index and thereto associated market data
    (let ((mdw (obj-method-wrap market-data 'MarketDataWithDates.Dated))
          (tzw (obj-method-wrap (TZ.New tz-name) 'TZ.T)))
      (let* ((hf-date (obj-method-wrap (mdw 'indexDate index) 'HFDate.T)) 
             (date    (tzw 'localtime (hf-date 'get))))
        (list (cdr (assoc 'year  date))
              (cdr (assoc 'month date)) 
              (cdr (assoc 'day   date)) 
)))))

    
