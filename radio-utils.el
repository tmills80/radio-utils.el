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

;; radio-utils is a collection of utilities I have written for using
;; and logging my radio listening and ham radio activities in Emacs
;; The log file is an org-mode file, radio control is achieved using
;;rigctl from Hamlib


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

(defvar radio-default-radio
  nil
  "`radio-definition' of the default radio.")

;;;; Functions

(cl-defstruct radio-definition
  "Definition of a radio."
  (name nil :documentation "A human friendly name for the radio.")
  (rig-number nil :documentation "The rig number from Hamlib.")
  (port nil :documentation "The port to use: eg COM1, 192.168.64.1:4532")
  (antenna nil :documentation "The antenna in use"))

(defun radio--send-command (radio command)
  "Send a `Hamlib' command COMMAND to radio RADIO."
  (let ((rig-number (radio-definition-rig-number radio))
	(port (radio-definition-port radio)))
    (shell-command-to-string
     (format "rigctl -m %d -r %s %s" rig-number port command))))

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
          ":FREQUENCY:%^{Frequency|" (radio-get-frequency radio) "}\n"
          ":MODE:%^{Mode|" (radio-get-mode radio) "}\n"
          ":RADIO:%^{Radio|" (radio-definition-name radio) "}\n"
          ":ANTENNA:%^{Antenna| " (radio-definition-antenna radio) "}\n"
          ":END:\n"
          "%?"))


(defun radio-log-template ()
  "Generate an `org-capture' template using `radio-default-radio'.
Provide this as an argument to an `org-capture-templates' entry."
  (radio--log-template radio-default-radio))

;;;;; Location functions

(defun radio--grid-to-ord (char)
  "Convert CHAR to its grid location value.
If CHAR is ?A - ?X then ?A = 0, ?B = 1 etc.
If CHAR is ?0 - ?9 then return the numeric value."
  (cond ((and (>= char ?A) (<= char ?X)) (- char ?A))
        ((and (>= char ?0) (<= char ?9)) (- char ?0))
        (t (error "Not a valid Maidenhead character %s" char))))

(defun radio--valid-grid-p (grid)
  "Test if GRID is a valid Maidenhead grid reference."
  (let ((regex "^[A-R][A-R]\\([0-9][0-9]\\|[0-9][0-9][A-X][A-X]\\)?$"))
    (string-match-p regex grid)))

(defun radio-grid-to-latlong (grid)
  "Convert Maidenhead GRID to its latitude and longitude.
Provides the SW corner of the area defined by the grid reference.
See `https://www.dxzone.com/grid-square-locator-system-explained/'
for an explanation."
  (if (not (radio--valid-grid-p grid))
      (error "Invalid Maidenhead grid reference %s" grid)
    (let* ((locations (mapcar #'radio--grid-to-ord grid))
           (len (length locations))
           (lat 0.0)
           (lon 0.0))
      ;; Field - 20 degrees longitude by 10 degrees lattitude
      ;;   Longitude A is at +/-180 (half way round the world from Greenwich)
      ;;   Latitude A is at -90 (ie south pole)
      (setq lat (+ -90 (* 10 (nth 1 locations))))
      (setq lon (+ -180 (* 20 (nth 0 locations))))
      ;; Square - 2 degrees longitude by 1 degree lattitude
      (if (> len 2)
          (progn
            (setq lat (+ lat (nth 3 locations)))
            (setq lon (+ lon (* 2 (nth 2 locations))))
            ;; Subsquare - 5 arcminutes longitude by 2.5 arcminutes latitude
            (if (> len 4)
                (progn
                  (setq lat (+ lat
                               (* (nth 5 locations) (/ 2.5 60))
                               (/ 1.25 60)))
                  (setq lon (+ lon
                               (* (nth 4 locations) (/ 5.0 60))
                               (/ 2.5 60))))
              ;; else find center of square
              (progn
                (setq lat (+ lat 0.5))
                (setq lon (+ lon 1)))))
        ;; else find middle of field
        (progn
          (setq lat (+ lat 5))
          (setq lon (+ lon 10))))
      (cons lat lon))))

(defun radio-latlong-to-grid (latlong)
  "Convert a latitude and longitude to a Maidenhead 3rd level grid square.
LATLONG should be a cons cell with the latitude and longitude.
See `https://www.dxzone.com/grid-square-locator-system-explained/' for an
explanation."
  (let* ((arcminute (/ 1 60.0))
         (lat (+ 90 (car latlong)))
         (lon (+ 180 (cdr latlong)))
         (field-lon (floor (/ lon 20)))
         (field-lat (floor (/ lat 10)))
         (square-lon (floor (/ (mod lon 20) 2)))
         (square-lat (floor (mod lat 10)))
         (sub-lon (floor (/ (mod (mod lon 20) 2) (* 5 arcminute))))
         (sub-lat (floor (/ (mod (mod lat 10) 1) (* 2.5 arcminute)))))
    (cl-flet ((get-char (val) (char-to-string (+ ?A val))))
      (format "%s%s%d%d%s%s"
              (get-char field-lon)
              (get-char field-lat)
              square-lon
              square-lat
              (get-char sub-lon)
              (get-char sub-lat)))))

(defun radio--latlong-distnace (location1 location2)
  "Calculate distances between two points of latitude and longitude in km.
LOCATION1 and LOCATION2 are cons cells of the location

Do not use for high precision calculations or two points close together.

Uses spherical law of cosines:
    `https://en.wikipedia.org/wiki/Great-circle_distance#Formulae'
assuming that the platform Emacs is running on uses IEEE binary64
and as the precision of Maidenhead Grid is not down to a few meters
this will result in a low chance of a rounding error."
  (let ((lat1 (degrees-to-radians (car location1)))
        (lon1 (degrees-to-radians (cdr location1)))
        (lat2 (degrees-to-radians (car location2)))
        (lon2 (degrees-to-radians (cdr location2)))
        (r 6371))
    (* r
       (acos (+ (* (sin lat1) (sin lat2))
                (* (cos lat1)
                   (cos lat2)
                   (cos (abs (- lon2 lon1)))))))))

(defun radio-distance (grid1 grid2)
  "Calculate distance between two grid squares, GRID1 and GRID2, in km.
This is approximate, assuming a spherical earth as the precision needed for my
purposes is not high."
  (radio--latlong-distnace (radio-grid-to-latlong grid1)
                           (radio-grid-to-latlong grid2)))

(provide 'radio-utils)
;;; radio-utils.el ends here
