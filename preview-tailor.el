;;; preview-tailor.el --- Tailor AUCTeX preview scale to monitor/text scale  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/preview-tailor.el
;; Package-Requires: ((emacs "29.1"))
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

;;; Code:

(require 'face-remap)
(require 'preview)

(defcustom preview-tailor-multipliers
  '((() . 1.0))

  "Alist mapping lists of monitor attributes to multipliers.
Each monitor is described by a list of its attributes, which
should be a subset of those returned by
`frame-monitor-attributes'.  The empty list of attributes ()
matches against everything, hence functions as a default."
  :group 'AUCTeX
  :type '(alist :key-type (repeat (sexp  :tag "Monitor Attribute"))
                :value-type (number :tag "Preview Scale")))

(defun preview-tailor--get-match (attr-list)
  "Return preview scale corresponding to attribute list ATTR-LIST.
Uses the customization variable `preview-tailor-multipliers'.  Returns
nil if no match found."
  (cl-find-if (lambda (pair)
                (let ((monitor-attr-list (car pair)))
                  (cl-every (lambda (attr)
                              (member attr attr-list))
                            monitor-attr-list)))
              preview-tailor-multipliers))

(defun preview-tailor--get-multiplier ()
  "Get the preview scale multiplier for the current monitor."
  (or (cdr (preview-tailor--get-match (frame-monitor-attributes)))
      1.5))

(defcustom preview-tailor-additional-factor-function nil
  "Function to calculate an additional factor for preview scale.
This function should take no arguments and returns a number. The
returned number is used as a multiplication factor in
`preview-tailor--calculate'.  If nil, then the no additional
factor is used."
  :group 'AUCTeX
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function" identity)))

(defun preview-tailor--calculate ()
  "Calculate the AUCTeX preview scale.
This calculation is based on four factors:

- Result of `preview-scale-from-face'.

- Current text scale factor (adjusted via `text-scale-adjust').

- Multiplier from `preview-tailor-multipliers' for current
  monitor.

- Result of `preview-tailor-additional-factor-function', if
  non-nil."
  (let* ((face-scale (funcall (preview-scale-from-face)))
         (text-scale (expt text-scale-mode-step text-scale-mode-amount))
         (monitor-multiplier (preview-tailor--get-multiplier))
         (additional-factor (or (and preview-tailor-additional-factor-function
                                     (funcall preview-tailor-additional-factor-function))
                                1)))
    (* preview-scale text-scale monitor-multiplier additional-factor)))

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

;;;###autoload
(defun preview-tailor-init ()
  "Initialize preview-tailor."
  (interactive)
  (setq preview-scale-function #'preview-tailor--calculate))

;;;###autoload
(defun preview-tailor-save ()
  "Save preview-tailor customization."
  (interactive)
  (customize-save-variable 'preview-tailor-multipliers preview-tailor-multipliers))

(provide 'preview-tailor)
;;; preview-tailor.el ends here
