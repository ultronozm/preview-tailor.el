;;; preview-tailor.el --- Tailor AUCTeX preview scale to monitor/text scale  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/preview-tailor.el
;; Package-Requires: ((emacs "29.1") (auctex "11.86.1"))
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

(defun preview-tailor--calculate ()
  "Calculate the AUCTeX preview scale.
We take this to be the product of three factors:

- the result of preview-scale-from-face

- the current text scale factor (e.g., tweaked via
  `text-scale-adjust')

- the multiplier for the current monitor, determined via the
  alist `preview-tailor-multipliers'."
  (*
   (funcall (preview-scale-from-face))
   (expt text-scale-mode-step text-scale-mode-amount)
   (preview-tailor--get-multiplier)))

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

(provide 'preview-tailor)
;;; preview-tailor.el ends here
