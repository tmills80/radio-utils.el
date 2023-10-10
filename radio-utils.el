;;; radio-utils.el --- Utilities for Radio Monitoring and Ham Radio -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Tristan Mills

;; Author: Tristan Mills <tristan@eridu.org.uk>
;; URL: https://github.com/tmills80/radio-utils.el
;; Package-Version: 0.1-pre
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: external

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; radio-utils is a collection of utilities I have written for using and logging my radio
;; listening and ham radio activities in Emacs
;; The log file is an org-mode file, radio control is achieved using rigctl from Hamlib


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)

;;;; Customization

(defgroup radio ()
  "Radio control and logging utilities."
  :group 'external)

(defcustom radio-log-file
  nil
  "Default org file for logging to."
  :type 'file
  :group 'radio)

(defvar radio-default-radio)

;;;; Functions

(cl-defstruct radio-definition
  "Definition of a radio."
  (name nil :documentation "A human friendly name for the radio.")
  (rig-number nil :documentation "The rig number from Hamlib.")
  (port nil :documentation "The port to use: eg COM1, 192.168.64.1:4532"))


(defun radio--send-command (radio command)
  "Send a `Hamlib' command COMMAND to radio RADIO."
  (let ((rig-number (radio-definition-rig-number radio))
	(port (radio-definition-port radio)))
    (shell-command-to-string (format "rigctl -m %d -r %s %s" rig-number port command))))

(defun radio-utc-time (&optional time format)
  "Get the time in UTC.
TIME is the time to convert in the system time zone or nil for current time
FORMAT is the `format-time-string' format, or [%F %a %H %R] if nil"
  (let ((format (or format "[%F %a %H %R]")))
    (format-time-string format time "UTC0")))

(defun radio-insert-utc ()
  "Insert the current date and time in UTC as an org inactive timestamp."
  (interactive)
  (insert (radio-utc-time)))

(defun radio-open-log (&optional filename)
  "Open the log file.
With optional FILENAME open that file instead of `radio-log-file'."
  (interactive)
  (let ((filename (or filename radio-log-file)))
    (find-file filename)))

(defun radio--get-frequency-hz (radio)
  "Get the current frequency in Hz from radio-description RADIO."
  (string-to-number (radio--send-command radio "get_freq")))

(defun radio--format-frequency (frequency)
  "Format FREQUENCY given in Hz.
Less than 30MHz frequency is in kHz.
Greater than 30MHz is in MHz, unless greater than 1GHz when GHz is used."
  (cond ((< frequency 30e6) (format "%g kHz" (/ frequency 1e3)))
        ((< frequency 1e9) (format "%g MHz" (/ frequency 1e6)))
        (t (format "%g GHz" (/ frequency 1e9)))))

(defun radio-get-frequency (&optional radio hz)
  "Get the frequency RADIO is tuned to in a human readable form.
If optional RADIO is not supplied then use `radio-default-radio'.
If optional HZ is supplied then return the frequency in Hz."
  (let* ((radio (or radio radio-default-radio))
         (frequency (radio--get-frequency-hz radio)))
    (if hz
        frequency
      (radio--format-frequency frequency))))

(defun radio-get-mode (&optional radio)
  "Get the current mode of RADIO.
If optional RADIO is nil then use `radio-default-radio'."
  (let ((radio (or radio radio-default-radio)))
    (car (split-string (radio--send-command radio "get_mode")))))

(defun radio--log-template (radio)
  "Generate an org template using the information from radio-description RADIO."
  (concat "* " (radio-utc-time) " %^{Station}\n"
          ":PROPERTIES:\n"
          ":START-TIME:" (radio-utc-time) "\n"
          ":FREQUENCY:" "%^{Frequency|" (radio-get-frequency radio) "}\n"
          ":MODE:" "%^{Mode|" (radio-get-mode radio) "}\n"
          ":END:\n"
          "%?"))


(defun radio-log-template ()
  "Generate an `org-capture' template using `radio-default-radio'.
Provide this as an argument to an `org-capture-templates' entry."
  (radio--log-template radio-default-radio))

(provide 'radio-utils)
;;; radio-utils.el ends here
