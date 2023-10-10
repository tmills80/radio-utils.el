;; TODO add more sources - each with their own capture template

;; SDR Console in Windows: run rigctld.exe -m 2014 -r COM2 in windows to expose this
(setq radio-default-radio (make-radio-definition :name "SDR Console" :port "192.168.64.1:4532" :rig-number 2))
;; Set the log file
(setq radio-log-file "~/org/RadioLog.org")

;; Add the capture template
(add-to-list 'org-capture-templates
             '("l" "Radio log entry" entry (file radio-log-file)
               (function radio-log-template)))
