;;; stub/avy.el --- ??? -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

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

(defvar avy-keys nil)
(defvar avy-single-candidate-jump nil)
(defvar avy-handler-function nil)
(defvar avy-pre-action nil)
(defvar avy-all-windows nil)
(defvar avy-style nil)
(defvar avy-action nil)

(defun avy-process (candidates overlay-fn))
(defun avy--process (candidates overlay-fn))
(defun avy--style-fn (style))

(provide 'avy) ;; FIXME: Really?
;;; stub/posframe.el ends here
