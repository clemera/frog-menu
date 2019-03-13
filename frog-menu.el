;;; frog-menu.el --- Quickly pick items from ad hoc menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Clemens Radermacher

;; Author: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/clemera/frog-menu
;; Version: 0.1
;; Package-Requires: ((emacs "26") (avy) (posframe))
;; Keywords: convenience

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
;;
;; This package lets you quickly pick strings from ad hoc menus. Just like a
;; frog would catch a fly. The menu is built "on the fly" from a collection of
;; strings. It's presented to the user to choose one of them. One example
;; where this kind of menu is useful are spelling correction suggestions.
;;
;; To invoke the menu users can call `frog-menu-read'. How items are displayed
;; and chosen depends on `frog-menu-type'. The default type `avy-posframe'
;; uses `avy' and `posframe'. Their handler functions can be used as reference
;; if you want to define a new `frog-menu-type'.
;;
;; Here is an example how you would use `frog-menu-read' to implement a
;; `flyspell-correct-interface':
;;
;; (defun frog-menu-flyspell-correct (candidates word)
;;   "Run `frog-menu-read' for the given CANDIDATES.
;;
;; List of CANDIDATES is given by flyspell for the WORD.
;;
;; Return selected word to use as a replacement or a tuple
;; of (command . word) to be used by `flyspell-do-correct'."
;;   (let* ((corrects (if flyspell-sort-corrections
;;                        (sort candidates 'string<)
;;                      candidates))
;;          (actions `(("C-s" "Save word"         (save    . ,word))
;;                     ("C-a" "Accept (session)"  (session . ,word))
;;                     ("C-b" "Accept (buffer)"   (buffer  . ,word))
;;                     ("C-c" "Skip"              (skip    . ,word))))
;;          (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
;;                                                    ispell-dictionary
;;                                                    "default")))
;;          (res      (frog-menu-read prompt corrects actions)))
;;     (unless res
;;       (error "Quit"))
;;     res))
;;
;; (setq flyspell-correct-interface #'frog-menu-flyspell-correct)
;;
;;; Code:

(require 'avy)
(require 'posframe)

(defgroup frog-menu nil
  "Quickly pick items from ad hoc menus."
  :group 'convenience
  :prefix "frog-menu-")

(defcustom frog-menu-type 'avy-posframe
  "The type of menu to be used.

When defining a new menu type, handlers need to be added for

`frog-menu-init-handler-alist'

`frog-menu-display-handler-alist'

`frog-menu-query-handler-alist'

and optionally to

`frog-menu-cleanup-handler-alist'."
  :type 'symbol)


(defcustom frog-menu-init-handler-alist
  '((avy-posframe . frog-menu-init-avy-posframe))
  "Maps `frog-menu-type' to an init handler.

The init handler is called with prompt, strings and actions
arguments of `frog-menu-read'. It should initialize the buffer for
display and return it. Afterwards the hook
`frog-menu-after-init-hook' gets executed."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-display-handler-alist
  '((avy-posframe . frog-menu-display-posframe))
  "Maps `frog-menu-type' to a display handler.

The display handler receives the buffer to display as an argument
and should return the candidates to pass as first argument to the
query handler."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-query-handler-alist
  '((avy-posframe . frog-menu-avy-posframe))
  "Maps `frog-menu-type' to a query handler.

The query handler receives two arguments.

The first are the candidates passed from the return value of the
display handler. The second one is the actions argument passed to
`frog-menu-read'.

This function should return the chosen string or action return
value. If the user exited the query return nil."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-cleanup-handler-alist
  '((avy-posframe . posframe-hide))
  "Maps `frog-menu-type' to a cleanup handler.

The cleanup handler receives the displayed buffer as argument and
is called after the query handler returns or exits through an
error."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-after-init-hook '()
  "Frog menu init hook.

Runs after menu buffer is initialized by its init handler. The
menu buffer is set current when this hook runs."
  :type '(repeat function))

(defcustom frog-menu-grid-width-function
  (lambda () (/ (frame-width) 2))
  "Returns the width that should be used for menu grid."
  :type 'function)

(defcustom frog-menu-grid-column-function
  (lambda ()
    (/ (funcall frog-menu-grid-width-function)
       8))
  "Returns the number of columns for the menu grid.

Less columns are used automatically if the grid width is not big
enough to contain that many columns."
  :type 'function)

(defcustom frog-menu-avy-keys (append (string-to-list "asdflkjgh")
                                      (string-to-list "qwerpoiuty")
                                      (string-to-list "zxcvmnb")
                                      (string-to-list (upcase "asdflkjgh"))
                                      (string-to-list (upcase "qwerpoiuty"))
                                      (string-to-list (upcase "zxcvmnb"))
                                      (number-sequence ?, ?@))
  "Frog menu keys used for `avy-keys'.

By default uses a large collection of keys, so that the hints can
be drawn by single characters."
  :type '(repeat character))

(defface frog-menu-border '((((background dark))  . (:background "white"))
                            (((background light)) . (:background "black")))
  "The border color for the posframe.")

(defface frog-menu-candidate-face
  '((t (:inherit default)))
  "Face used for menu candidates.")

(defvar frog-menu--buffer " *frog-menu-menu*"
  "Buffer used for the frog menu.")

;; * Init

(defun frog-menu--init-buffer (prompt strings actions)
  "Initialize the menu buffer and return it.

PROMPT STRINGS and ACTIONS are the args from `frog-menu-read'."
  (with-current-buffer (get-buffer-create frog-menu--buffer)
    (erase-buffer)
    (funcall (cdr (assq frog-menu-type
                        frog-menu-init-handler-alist))
             prompt strings actions)
    (run-hooks 'frog-menu-after-init-hook)
    (get-buffer frog-menu--buffer)))

(defun frog-menu-init-avy-posframe (prompt strings actions)
  "Init handler for avy-posframe.

PROMPT, STRINGS and ACTIONS are the args from `frog-menu-read'.

Fills the buffer with a grid of STRINGS followed by PROMPT and
ACTIONS."
  (when strings
    (insert
     (frog-menu--grid-format
      strings
      (funcall frog-menu-grid-column-function)
      (funcall frog-menu-grid-width-function)))
    (insert "\n\n"))
  (insert (frog-menu--prompt-format prompt actions))
  ;; padding for avy char
  (goto-char (point-min))
  (while (not (eobp))
    (goto-char (line-end-position))
    (insert " ")
    (forward-line 1))
  ;; posframe needs point at start,
  ;; otherwise it fails on first init
  (goto-char (point-min)))


;; * Formatting

(defun frog-menu--grid-format (strings cols &optional width)
  "Return grid string built with STRINGS.

The grid will be segmented into columns. COLS is the maximum
number of columns to use. The columns have WIDTH space in
horizontal direction which default to frame width.

Returns the buffer containing the formatted grid."
  (with-temp-buffer
    (let* ((length (apply 'max
                          (mapcar #'string-width strings)))
           (wwidth (or width (frame-width)))
           (columns (min cols (/ wwidth (+ 2 length))))
           (colwidth (/ wwidth columns))
           (column 0)
           (first t)
           laststring)
      (dolist (str strings)
        (unless (equal laststring str)
          (setq laststring str)
          (let ((length (string-width str)))
            (unless first
              (if (or (< wwidth (+ (max colwidth length) column))
                      (zerop length))
                  (progn
                    (insert "\n" (if (zerop length) "\n" ""))
                    (setq column 0))
                (insert " \t")
                (set-text-properties (1- (point)) (point)
                                     `(display (space :align-to ,column)))))
            (setq first (zerop length))
            (add-text-properties (point)
                                 (progn (insert str)
                                        (point))
                                 '(face frog-menu-candidate-face))
            (setq column (+ column
                            (* colwidth (ceiling length colwidth)))))))
      (buffer-string))))


(defun frog-menu--prompt-format (prompt actions)
  "Return string containing formatted PROMPT and ACTIONS.

PROMPT and ACTIONS are the arguments of `frog-menu-read'."
  (with-temp-buffer
    (insert prompt "\n")
    (let ((header-pos (point)))
      (dolist (action actions)
        (insert (car action)
                "_"
                (replace-regexp-in-string " " "_" (cadr action))
                " "))
      (insert "\n")
      (let ((fill-column (1+ (funcall frog-menu-grid-width-function))))
        (fill-region header-pos (point))
        (align-regexp header-pos (point) "\\(\\s-*\\) " 1 1 nil)
        (while (re-search-backward "_" header-pos t)
          (replace-match " "))))
    (goto-char (point-min))
    (buffer-string)))

;; * Display

(defun frog-menu-display-posframe (buf)
  "Display posframe showing buffer BUF.

Returns candidates to be handled by query handler."
  (posframe-show buf
                 :poshandler #'posframe-poshandler-point-bottom-left-corner
                 :internal-border-width 1)
  (set-face-attribute 'internal-border
                      (buffer-local-value 'posframe--frame buf)
                      :inherit 'frog-menu-border)
  (frog-menu--get-avy-candidates
   (frame-selected-window
    (buffer-local-value 'posframe--frame buf))
   buf))

(defun frog-menu--get-avy-candidates (&optional w b start end)
  "Return candidates to be passed to `avy--process'.

W is the window where the candidates can be found and defaults to
the currently selected one. B is the buffer of the candidates and
defaults to the current one. START and END are the buffer
positions containing the candidates and default to ‘point-min’ and
‘point-max’."
  (let ((w (or w (selected-window)))
        (b (or b (current-buffer)))
        (candidates ()))
    (with-current-buffer b
      (let ((start (or start (point-min)))
            (end (or end (point-max))))
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (when (eq (get-char-property (point) 'face)
                      'frog-menu-candidate-face)
              (push (cons (point) w) candidates))
            (goto-char
             (or (next-single-property-change
                  (point) 'face)
                 (point-max)))
            (while (< (point) (point-max))
              (unless (looking-at "[[:blank:]\r\n]\\|\\'")
                (push (cons (point) w)
                      candidates))
              (goto-char
               (or (next-single-property-change
                    (point)
                    'face)
                   (point-max))))))))
    (nreverse candidates)))

;; * Query handler functions

(defvar frog-menu--avy-action-map nil
  "Internal keymap saving the actions for the avy handler.")

(defun frog-menu--posframe-ace-handler (char)
  "Execute menu action for CHAR."
  (cond ((memq char '(27 ?\C-g))
         ;; exit silently
         (throw 'done 'exit))
        ((mouse-event-p char)
         (signal 'user-error (list "Mouse event not handled" char)))
        (t
         (require 'edmacro)
         (let* ((key (kbd (edmacro-format-keys (vector char))))
                (cmd (lookup-key frog-menu--avy-action-map key)))
           (if (commandp cmd)
               (throw 'done (list (list cmd)))
             (message "No such candidate: %s, hit `C-g' to quit."
                      (if (characterp char) (string char) char))
             (throw 'done 'restart))))))

(defun frog-menu--init-avy-action-map (actions)
  "Initialize `frog-menu--action-map'.

Each action of ACTIONS is bound to a command which returns the
action result. ACTIONS is the argument of `frog-menu-read'."
  (setq frog-menu--avy-action-map (make-sparse-keymap))
  (dolist (action actions)
    (define-key frog-menu--avy-action-map (kbd (car action))
      (lambda () (interactive) (car (cddr action))))))

(defun frog-menu-avy-posframe (candidates actions buffer)
  "Query handler for avy-posframe.

CANDIDATES are the candidates for `avy--process'. ACTIONS is the
argument of `frog-menu-read'. BUFFER is the menu buffer which
gets hidden after the query."
  ;; uses a global keymap var to pass info to avy handler
  (frog-menu--init-avy-action-map actions)
  (if candidates
      (let* ((avy-keys frog-menu-avy-keys)
             (avy-single-candidate-jump (null actions))
             (avy-handler-function #'frog-menu--posframe-ace-handler)
             (avy-pre-action #'ignore)
             (avy-all-windows nil)
             (avy-style 'pre)
             (avy-action #'identity)
             (pos (avy--process
                   candidates
                   (avy--style-fn avy-style))))
        (cond ((number-or-marker-p pos)
               ;; string
               (with-current-buffer buffer
                 (let* ((start pos)
                        (end (or (next-single-property-change start 'face)
                                 (point-max))))
                   (buffer-substring start end))))
              ((commandp pos)
               ;; action
               (call-interactively pos))))
    (let ((cmd (lookup-key frog-menu--avy-action-map (vector (read-char)))))
      (when (commandp cmd)
        (call-interactively cmd)))))


;; * Entry point


(defun frog-menu-read (prompt strings &optional actions)
  "Read from a menu of `frog-menu-type'.

PROMPT is a string with prompt information for the user.

STRINGS is a list of strings from which the user is expected to
choose one.

ACTIONS is an additional list of actions that can be given to let
the user choose an action instead of returning a chosen string.

Each ACTION is a list of the form:

    (KEY DESCRIPTION RETURN)

Where KEY is a string to be interpreted as spelled-out
keystrokes, using the same format as for `kbd'.

DESCRIPTION is a string to be displayed along with KEY to
describe the action.

RETURN will be the returned value if KEY is pressed."
  (let* ((buf (frog-menu--init-buffer prompt strings actions))
         (dhandler (cdr (assq frog-menu-type
                              frog-menu-display-handler-alist)))
         (candidates (funcall dhandler buf))
         (qhandler (cdr (assq frog-menu-type
                              frog-menu-query-handler-alist)))
         (cuhandler (cdr (assq frog-menu-type
                               frog-menu-cleanup-handler-alist)))
         (res nil))
    (unwind-protect
        (setq res (funcall qhandler candidates actions buf))
      (when cuhandler
        (funcall cuhandler buf)))
    res))



(provide 'frog-menu)
;;; frog-menu.el ends here
