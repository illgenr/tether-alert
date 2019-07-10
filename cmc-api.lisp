(progn 
  (ql:quickload :log4cl)
  (ql:quickload :drakma)
  (ql:quickload :cl-ppcre)
  (ql:quickload :yason)
  (require 'uiop))

; Set these variables
(defparameter cmc-api-key nil) ; x-x-x-x-x
(defparameter to-recipient "nobody@nowhere.com")

(defparameter resp nil)
(defparameter current-tether-supply nil)
(defparameter last-tether-supply nil)
(defparameter tether-id "825")

(defvar to-header "to:")
(defvar subject-header "subject:Tether Supply Alert")
(defvar from-header "from:tether-alerts")

(defparameter activation-message "The tether supply has changed by ~a~%The total is ~a")

(defun api-call ()
  (let ((stream (drakma:http-request "https://pro-api.coinmarketcap.com/v1/cryptocurrency/quotes/latest"
			      :method :get
			      :parameters `(("id" . ,tether-id))
			      :additional-headers `(("Accepts" . "application/json")
						    ("X-CMC_PRO_API_KEY" . ,cmc-api-key))
			      :want-stream t)))
		 (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
		 (yason:parse stream :object-as :hash-table)))

(defun get-supply-int (hash-table)
  (floor (gethash '"total_supply" (gethash '"825" (gethash '"data" hash-table)))))

(defun make-message-stream (diff total)  
  (let ((message (format nil activation-message diff total)))
    (make-string-input-stream (format nil "~a~%~a~%~%~a~%" from-header subject-header message))))

(defun send-message (diff total)
    (log:info "~%diff: ~a~%total: ~a~%" diff total)
    (uiop:launch-program
     (list "sendmail" "-v" (concatenate 'string to-header to-recipient) "-f" from-header)
     :input (make-message-stream diff total)))

(defun timer-callback ()
  (defparameter difference 0)
  (setq current-tether-supply (get-supply-int (api-call)))
  (let ((difference (- current-tether-supply last-tether-supply)))
    (if (not (eq difference 0))
	(progn
	  (print "supply change!")
	  (send-message difference current-tether-supply))
	(print "No change"))      
    (setq last-tether-supply current-tether-supply)))
  

(defun setup-api-timer ()
  (sb-ext:schedule-timer (sb-ext:make-timer #'timer-callback :name "tether-api-timer") 1 :repeat-interval 300))

(defun unschedule-api-timer ()
  (loop for timer in (sb-ext:list-all-timers)
     do (if (equal (sb-ext:timer-name timer) "tether-api-timer")
		   (sb-ext:unschedule-timer timer))))

(defun main ()
  (if (equal cmc-api-key nil)
      (print "Coinmarketcap API key not set")
      (progn
	(setq current-tether-supply (get-supply-int (api-call)))
	(setq last-tether-supply current-tether-supply)
	(setup-api-timer))))
