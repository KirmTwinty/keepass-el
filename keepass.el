;;; keepass.el --- kpcli to Emacs interface

;; Copyright (C) 2019 Toullier Thibaud <thibaud.toullier at inria.fr>
;;
;; This program is not part of Gnu Emacs
;;
;; keepass.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;; This file has been created to enable the password reading from keepass
;; databases, helped with the kpcli tool.

;;; Code:
;; Requirements
(require 'popup)

(defgroup keepass nil
  "Get passwords from Keepass database."
  :group 'lisp
:prefix "keepass-")
;; Custom settings
(defcustom keepass-database-path "/home/ttoullie/Dropbox/KP2A/passDb.kdbx"
  "Properties of org file to show in the buffer."
  :type 'string
  :group 'keepass)

;; Variables
(defvar keepass-kpcli-enabled nil
  "True if kpcli is actually running.")
(defvar keepass-kpcli-buffer-name "*KPCLI*"
  "Name of the kpcli shell buffer.")
(defvar keepass-kpcli-buffer nil
  "The kpcli process holder.")
(defvar keepass-output nil
  "Output of the kpcli buffer.")
(defvar keepass-data nil
  "Database of keepass.")

(defun keepass-filter-output (process output)
  "Replace the keepass-output with the PROCESS last OUTPUT."
  ;; The default (add-function :before (process-filter (get-buffer-process keepass-kpcli-buffer-name)) 'keepass-filter-output) is not working
  (comint-output-filter process output) ;; hack here
  (setq keepass-output output))

(defun keepass-send (cmd)
  "Send a command CMD to the running kpcli."
  (if (buffer-live-p keepass-kpcli-buffer)
      (process-send-string keepass-kpcli-buffer (concat cmd "\n"))
    (message "Error, kpcli not launched")))

(defun keepass-launch ()
  "Launch kpcli if not running."
  (interactive)
  ;; if keepass data not nil and process is running
  (if (and keepass-data (buffer-live-p keepass-kpcli-buffer))
      ;; we show the popup
	(let ((selection (popup-cascade-menu keepass-data)))
	  (keepass-send (concat "cd '" (car selection) "'"))
	  (keepass-send "ls") ;; force refresh
	  (keepass-send (concat "xp " (cadr selection)))
	  (keepass-send (concat "cd /")))
    ;; Otherwise we launch the process
    (let ((keepass-pass (read-passwd "Secret passphrase? ")))
      (setq keepass-kpcli-buffer (shell keepass-kpcli-buffer-name))
      (set-process-filter (get-buffer-process keepass-kpcli-buffer) 'keepass-filter-output)
      (keepass-send "kpcli")
      (keepass-send (concat "open " keepass-database-path))
      (keepass-send keepass-pass)
      (sit-for 4)
      (setq keepass-data (keepass-parse-entries))
      (keepass-launch))))

(defun keepass-parse-entries(&optional tree)
  "Parse the keepass database to popup entries."
  (keepass-send "ls") ;; get the output
  (sit-for 0.2)
  (let ((lines (split-string keepass-output "\n"))
	(entry-type 0)
	(value))
    (dolist (el lines value)
      (if (and (> (length el) 5) (not (string= (substring el 0 5) "kpcli")))
	  (if (string= el "=== Groups ===")
	      (progn (setq entry-type 1))
	    (if (string= el "=== Entries ===")
		(setq entry-type 2)
	      (if (= entry-type 1)
		  (progn
		    (keepass-send (concat "cd '" el "'"))
		    (if tree
			(add-to-list 'tree el t)
		      (setq tree (list (concat "/" el))))
		    (add-to-list 'value (cons el (keepass-parse-entries tree)))
		    (setq tree (delete el tree))
		    (keepass-send "cd .."))
		(if (= entry-type 2)
		    (if value
			(add-to-list 'value (keepass-create-item el tree) t)
		      (setq value (list (keepass-create-item el tree))))))))))))

(defun keepass-create-item (el &optional tree)
  "Create a popup item for the element EL in the TREE of the database."
  ;; Take the first part of the string if more than 2 whitespaces
  (let ((str (car (split-string el "\\s-\\{2,\\}")))
	(num (car (split-string el "\\."))))
    ;; Remove number
    (setq str (cadr (split-string str "\\.")))
    (message str)
    ;; Remove extra whitespace
;;    (setq str (substring str 1 (length str)))
    ;; Limit ssize to 20
    (if (> (length str) 40)
	(setq str (concat (substring str 0 37) "...")))
;;    (popup-make-item str :value (list (combine-and-quote-strings tree) num))))
    (popup-make-item str :value (list (mapconcat 'identity tree "") num))))

(provide 'keepass)
;;; keepass.el ends here
