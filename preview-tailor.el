;;; preview-tailor.el --- Tailor AUCTeX preview scale to monitor/text scale  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.2
;; URL: https://github.com/ultronozm/preview-tailor.el
;; Package-Requires: ((emacs "29.1") (auctex))
;; Keywords: tex, multimedia

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

;; This package provides customization for AUCTeX preview scale based
;; on the monitor and text scale.
;;
;; To use it, add (preview-tailor-init) to your init file.  When you
;; want to change the preview scale for a given monitor, use
;;
;;   M-x preview-tailor-set-multiplier
;;
;; When you are happy with your settings and want to save them for
;; future sessions, use
;;
;;   M-x preview-tailor-save
;;
;; See README.org for further details.

;;; Code:

(require 'face-remap)
(require 'latex)
(require 'preview)

(defgroup preview-tailor nil
  "Tailor AUCTeX preview scale to monitor/text scale."
  :group 'preview)

(defvar preview-tailor-multipliers
  '((() . 1.0))

  "Alist mapping lists of monitor attributes to multipliers.
Each monitor is described by a list of its attributes, which
should be a subset of those returned by
`frame-monitor-attributes'.  The empty list of attributes ()
matches against everything, hence functions as a default.")

(defun preview-tailor--get-match (attribute-list)
  "Return preview scale corresponding to ATTRIBUTE-LIST.
Uses the customization variable `preview-tailor-multipliers'.  Returns
nil if no match found."
  (cl-find-if (lambda (pair)
                (let ((monitor-attr-list (car pair)))
                  (cl-every (lambda (attr)
                              (member attr attribute-list))
                            monitor-attr-list)))
              preview-tailor-multipliers))

(defun preview-tailor--get-multiplier ()
  "Get the preview scale multiplier for the current monitor."
  (or (cdr (preview-tailor--get-match (frame-monitor-attributes)))
      ;; the above should match unless the user deletes the default
      ;; entry in `preview-tailor-multipliers'
      1.0))

(defcustom preview-tailor-additional-factor-function nil
  "Function to calculate an additional factor for preview scale.
This function should take no arguments and return a number.  The
returned number, if non-nil, is used as a factor in
`preview-tailor--calculate'."
  :group 'AUCTeX
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function" identity)))

(defvar-local preview-tailor-local-multiplier 1.0
  "Local preview scale multiplier.
Intended for use in buffer-local settings.")

(defun preview-tailor--calculate ()
  "Calculate scale for AUCTeX previews.
Product of five factors:
- Result of `preview-scale-from-face'.
- Current text scale factor (adjusted via `text-scale-adjust').
- Multiplier from `preview-tailor-multipliers' for current
  monitor.
- Result of `preview-tailor-additional-factor-function', if
  non-nil.
- The buffer-local variable `preview-tailor-local-multiplier'."
  (*
   (funcall (preview-scale-from-face))
   (expt text-scale-mode-step text-scale-mode-amount)
   (preview-tailor--get-multiplier)
   (if preview-tailor-additional-factor-function
       (funcall preview-tailor-additional-factor-function)
     1)
   preview-tailor-local-multiplier))

(defun preview-tailor--remove-frames (attr)
  "Remove the `frames' entry from the list ATTR."
  (seq-remove (lambda (item)
                (and (listp item)
                     (eq (car item) 'frames)))
              attr))

;;;###autoload
(defun preview-tailor-set-multiplier (&optional scale)
  "Set preview multiplier for current monitor.
Use SCALE if provided, otherwise prompt for it."
  (interactive (list
                (read-number (format "Enter multiplier (current: %s): "
                                     (preview-tailor--get-multiplier)))))
  (let* ((attr (preview-tailor--remove-frames (frame-monitor-attributes))))
    (if-let ((item (assoc attr preview-tailor-multipliers 'equal)))
        (setcdr item scale)
      (add-to-list 'preview-tailor-multipliers (cons attr scale)))))

(defconst preview-tailor-storage-file
  (expand-file-name ".preview-tailor" user-emacs-directory)
  "File name where settings are stored.")

(defun preview-tailor-load ()
  "Load preview-tailor customization from a dotfile."
  (when (file-exists-p preview-tailor-storage-file)
    (with-temp-buffer
      (insert-file-contents preview-tailor-storage-file)
      (setq preview-tailor-multipliers (read (current-buffer))))))

(defvar preview-tailor--initialized nil
  "Non-nil if preview-tailor has been initialized.")

;;;###autoload
(defun preview-tailor-init ()
  "Initialize preview-tailor."
  (interactive)
  (preview-tailor-load)
  (setq preview-scale-function #'preview-tailor--calculate)
  (setq preview-tailor--initialized t))

;;;###autoload
(defun preview-tailor-save ()
  "Save preview-tailor customization to a dotfile."
  (interactive)
  (when preview-tailor--initialized
    (with-temp-file preview-tailor-storage-file
      (prin1 preview-tailor-multipliers (current-buffer)))))

(provide 'preview-tailor)
;;; preview-tailor.el ends here
