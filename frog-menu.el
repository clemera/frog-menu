;;; frog-menu.el --- Quickly pick items from ad hoc menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/clemera/frog-menu
;; Version: 0.2.11
;; Package-Requires: ((emacs "26") (avy "0.4") (posframe "0.4"))
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
(eval-when-compile
  (require 'subr-x))

(defgroup frog-menu nil
  "Quickly pick items from ad hoc menus."
  :group 'convenience
  :prefix "frog-menu-")

(defvar frog-menu-type nil
  "Type of menu to use.

By default types `avy-posframe' and `avy-side-window' are possible.

  When using a new menu type, handlers need to be added for

  `frog-menu-init-handler-alist'

  `frog-menu-display-handler-alist'

  `frog-menu-display-option-alist'

  `frog-menu-query-handler-alist'

  and optionally to

  `frog-menu-cleanup-handler-alist'.")

(defcustom frog-menu-type-function #'frog-menu-type
  "Function which should return the variable `frog-menu-type' to be used.

See variable `frog-menu-type'"
  :type 'function)

(defcustom frog-menu-after-init-hook '()
  "Frog menu init hook.

Runs after menu buffer is initialized by its init handler. The
menu buffer is set current when this hook runs."
  :type '(repeat function))

(defcustom frog-menu-init-handler-alist
  '((avy-posframe . frog-menu-init-display-buffer)
    (avy-side-window . frog-menu-init-display-buffer))
  "Maps variable `frog-menu-type' to an init handler.

The init handler is called with the prompt, strings formatted by
`frog-menu-format-strings-function' actions formatted by
`frog-menu-format-actions-function'. It should initialize the
display buffer (which is current when called). After init the
hook `frog-menu-after-init-hook' gets executed."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-display-handler-alist
  '((avy-posframe . frog-menu-display-posframe)
    (avy-side-window . frog-menu-display-side-window))
  "Maps variable `frog-menu-type' to a display handler.

The display handler receives the buffer to display as an argument
and should return the window of the displayed buffer."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-display-option-alist
  '((avy-posframe . posframe-poshandler-point-bottom-left-corner)
    (avy-side-window . (display-buffer-in-side-window (side . bottom))))
  "Maps variable `frog-menu-type' to a display option.

The display option is passed to the display handler as second argument."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-query-handler-alist
  '((avy-posframe . frog-menu-query-with-avy)
    (avy-side-window . frog-menu-query-with-avy))
  "Maps variable `frog-menu-type' to a query handler.

The query handler receives four arguments.

The first is the displayed buffer. The second the window where
the buffer is displayed. The last one is the actions argument
passed to `frog-menu-read'.

This function should return the chosen string or action return
value. If the user exited the query return nil."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-cleanup-handler-alist
  '((avy-posframe . frog-menu-posframe-hide)
    (avy-side-window . frog-menu-side-window-hide))
  "Maps variable `frog-menu-type' to a cleanup handler.

The cleanup handler receives the displayed buffer and the window
as arguments and is called after the query handler returns or
exits through an error."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom frog-menu-avy-padding nil
  "If non-nil use padding between avy hints and candidates."
  :type 'boolean)

(defcustom frog-menu-posframe-border-width 1
  "Border width to use for the posframe `frog-menu' creates."
  :type 'integer)

(defcustom frog-menu-posframe-parameters nil
  "Explicit frame parameters to be used by the posframe `frog-menu' creates."
  :type 'list)

(defcustom frog-menu-format-actions-function #'frog-menu-action-format
  "Function used to format the actions passed to `frog-menu-read'."
  :type 'function)

(defcustom frog-menu-format-strings-function #'frog-menu-grid-format
  "Function used to format the strings passed to `frog-menu-read'."
  :type 'function)

(defcustom frog-menu-min-col-padding 2
  "Minimal padding between columns of grid."
  :type 'integer)

(defcustom frog-menu-grid-width-function
  (lambda () (cond ((eq (funcall frog-menu-type-function) 'avy-posframe)
                    (/ (frame-width) 2))
                   ((eq (funcall frog-menu-type-function) 'avy-side-window)
                    (* 2 (/ (frame-width) 3)))
                   (t (frame-width))))
  "Returns the width that should be used for menu grid.

Used by `frog-menu-grid-format'."
  :type 'function)

(defcustom frog-menu-grid-column-function
  (lambda ()
    (/ (funcall frog-menu-grid-width-function)
       8))
  "Returns the number of columns for the menu grid.

Less columns are used automatically if the grid width is not big
enough to contain that many columns.

Used by `frog-menu-grid-format' and `frog-menu-action-format'."
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

(defvar frog-menu-sort-function nil
  "A function to sort displayed strings for `frog-menu-read'.

If this variable is bound to a function `frog-menu-read' will
pass the strings to be displayed and the function to `sort':

    (let ((frog-menu-sort-function #'string<))
      (frog-menu-read \"Example\" '(\"z\" \"a\")))")

(defvar frog-menu-format completions-format
  "Defines in which order strings for `frog-menu-read' are displayed.

If the value is `vertical', strings are ordered vertically. If
the value is `horizontal', strings are ordered horizontally. This
variable does not define sorting, see `frog-menu-sort-function'
for this.")

(defface frog-menu-border '((((background dark))  . (:background "white"))
                            (((background light)) . (:background "black")))
  "The face defining the border for the posframe.")

(defface frog-menu-prompt-face
  '((t (:inherit default)))
  "Face used for menu promp")

(defface frog-menu-candidates-face
  '((t (:inherit default)))
  "Face used for menu candidates.")

(defface frog-menu-actions-face
  '((t (:inherit default)))
  "Face used for menu actions.")

(defface frog-menu-action-keybinding-face
  '((t (:inherit default)))
  "Face used for menu action keybindings.")

(defface frog-menu-posframe-background-face
  '((t :background "old lace"))
  "Face used for the background color of the posframe.")

(defvar frog-menu--buffer " *frog-menu-menu*"
  "Buffer used for the frog menu.")

(defun frog-menu-type ()
  "Return variable `frog-menu-type' to use."
  (cond ((display-graphic-p)
         'avy-posframe)
        (t
         'avy-side-window)))


;; * Init

(defun frog-menu--init-buffer (buffer prompt strings actions)
  "Initialize the menu BUFFER and return it.

PROMPT, STRINGS and ACTIONS are the args from `frog-menu-read'."
  (with-current-buffer buffer
    (erase-buffer)
    (let ((formats (and strings
                        (funcall frog-menu-format-strings-function
                                 strings)))
          (formata (and actions
                        (funcall frog-menu-format-actions-function
                                 actions))))
      (funcall (cdr (assq frog-menu-type
                          frog-menu-init-handler-alist))
               prompt
               formats
               formata)
      (run-hooks 'frog-menu-after-init-hook)
      buffer)))

(defun frog-menu-posframe-hide (buf _window)
  "Hide the posframe buffer BUF."
  (posframe-hide buf))

(defun frog-menu-side-window-hide (_buf window)
  "Hide the BUF side window WINDOW."
  (delete-window window))


(defun frog-menu-init-display-buffer (prompt
                                      formatted-strings
                                      formatted-actions)
  "Init handler for avy-posframe.

PROMPT, FORMATTED-STRINGS and FORMATTED-ACTIONS are the args from
`frog-menu-read'.

Fills the buffer with a grid of FORMATTED-STRINGS followed by PROMPT and
ACTIONS."
  (when formatted-strings
    (insert formatted-strings))
  (unless (string-empty-p prompt)
    (when formatted-strings
      (insert "\n\n"))
    (add-text-properties
     (point)
     (progn
       (insert prompt)
       (point))
     '(face frog-menu-prompt-face))
    (insert "\n"))
  (when formatted-actions
    (when (and formatted-strings
               (string-empty-p prompt))
      (insert "\n\n"))
    (insert formatted-actions))
  ;; posframe needs point at start,
  ;; otherwise it fails on first init
  (goto-char (point-min)))


;; * Formatting

(defun frog-menu-grid-format (strings)
  "Format STRINGS to a grid."
    (frog-menu--grid-format
     (mapcar (lambda (str)
               (concat (propertize
                        "_"
                        'face (list :foreground
                                (if (eq (funcall frog-menu-type-function)
                                        'avy-posframe)
                                    (face-background
                                     'frog-menu-posframe-background-face nil t)
                                  (face-background 'default))))
                       (if frog-menu-avy-padding " " "")
                       str)) strings)
     (funcall frog-menu-grid-column-function)
     (funcall frog-menu-grid-width-function)))

(defun frog-menu-action-format (actions)
  "Format ACTIONS for menu display."
  (when actions
    (with-temp-buffer
      (let ((header-pos (point)))
        (dolist (action actions)
          (add-text-properties
           (point)
           (progn
             (insert (car action))
             (point))
           '(face frog-menu-action-keybinding-face))
          (add-text-properties
           (point)
           (progn
             (insert "_"
                     (replace-regexp-in-string " " "_"
                                               (cadr action))
                     " ")
             (point))
           '(face frog-menu-actions-face)))
        (insert "\n")
        (let ((fill-column (1+ (funcall frog-menu-grid-width-function))))
          (fill-region header-pos (point))
          (align-regexp header-pos (point) "\\(\\s-*\\) " 1 1 nil)
          (while (re-search-backward "_" header-pos t)
            (replace-match " "))))
      (buffer-string))))

;; Taken partly from `completion--insert-strings'
(defun frog-menu--grid-format (strings cols &optional width)
  "Return grid string built with STRINGS.

The grid will be segmented into columns. COLS is the maximum
number of columns to use. The columns have WIDTH space in
horizontal direction which default to frame width.

Returns the formatted grid string."
  (with-temp-buffer
    (let* ((length (apply #'max
                          (mapcar #'string-width strings)))
           (wwidth (or width (frame-width)))
           (columns (max 1 (min cols
                                (/ wwidth
                                   (+ frog-menu-min-col-padding length)))))
           (colwidth (/ wwidth columns))
           (column 0)
           (first t)
           (rows (/ (length strings) columns))
           (row 0))
      (dolist (str strings)
        (let ((length (string-width str)))
          (cond ((eq frog-menu-format 'vertical)
                 ;; Vertical format
                 (when (> row rows)
                   (forward-line (- -1 rows))
                   (setq row 0 column (+ column colwidth)))
                 (when (> column 0)
                   (end-of-line)
                   (while (> (current-column) column)
                     (if (eobp)
                         (insert "\n")
                       (forward-line 1)
                       (end-of-line)))
                   (insert " \t")
                   (set-text-properties (1- (point)) (point)
                                        `(display (space :align-to ,column))))

                 (add-text-properties (point)
                                      (progn (insert str)
                                             (point))
                                      '(face frog-menu-candidates-face))

                 (if (> column 0)
                     (forward-line)
                   (insert "\n"))
                 (setq row (1+ row)))
                (t
                 ;; horizontal
                 (unless first
                   (if (or (< wwidth (+ (max colwidth length) column))
                           (zerop length))
                       (progn
                         (insert "\n" (if (zerop length) "\n" ""))
                         (setq column 0))
                     (insert " \t")
                     (set-text-properties
                      (1- (point))
                      (point)
                      `(display (space :align-to ,column)))))
                 (setq first (zerop length))
                 (add-text-properties (point)
                                      (progn (insert str)
                                             (point))
                                      '(face frog-menu-candidates-face))
                 (setq column (+ column
                                 (* colwidth (ceiling length colwidth))))))))
      (buffer-string))))


;; * Display

(defun frog-menu-display-posframe (buf &optional display-option)
  "Display posframe showing buffer BUF.

If given, DISPLAY-OPTION is passed as :poshandler to
`posframe-show'.

Returns window of displayed buffer."
  (posframe-show buf
                 :poshandler(or display-option
                                #'posframe-poshandler-point-bottom-left-corner)
                 :internal-border-width frog-menu-posframe-border-width
                 :background-color (face-attribute
                                    'frog-menu-posframe-background-face
                                    :background)
                 :override-parameters frog-menu-posframe-parameters)
  (set-face-attribute 'internal-border
                      (buffer-local-value 'posframe--frame buf)
                      :inherit 'frog-menu-border)
  (frame-selected-window
   (buffer-local-value 'posframe--frame buf)))

(defun frog-menu-display-side-window (buf &optional display-option)
  "Display posframe showing buffer BUF.

If given DISPLAY-OPTION is passed as action argument to
`display-buffer'.

Returns window of displayed buffer."
  (let ((window (display-buffer
                 buf
                 (or display-option
                     '(display-buffer-in-side-window (side . bottom))))))
    (prog1 window
      (with-selected-window window
        (with-current-buffer buf
          ;; see transient/lv
          (set-window-hscroll window 0)
          (set-window-dedicated-p window t)
          (set-window-parameter window 'no-other-window t)
          (setq window-size-fixed t)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (setq show-trailing-whitespace nil)
          (setq mode-line-format nil)
          (let ((window-resize-pixelwise t)
                (window-size-fixed nil))
            (fit-window-to-buffer nil nil 1))
          (goto-char (point-min)))))))


(defun frog-menu--get-avy-candidates (&optional b w start end)
  "Return candidates to be passed to `avy-process'.

B is the buffer of the candidates and defaults to the current
one. W is the window where the candidates can be found and
defaults to the currently selected one. START and END are the
buffer positions containing the candidates and default to
‘point-min’ and ‘point-max’."
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
                      'frog-menu-candidates-face)
              (push (cons (point) w) candidates))
            (goto-char
             (or (next-single-property-change
                  (point) 'face)
                 (point-max)))
            (while (< (point) (point-max))
              (unless (or (looking-at "[[:blank:]\r\n]\\|\\'")
                          (not (eq (get-char-property (point) 'face)
                                   'frog-menu-candidates-face)))

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
  (cond ((memq char '(?\e ?\C-g))
         ;; exit silently
         (throw 'done 'exit))
        ((mouse-event-p char)
         (signal 'user-error (list "Mouse event not handled" char)))
        (t
         (let* ((key (kbd (key-description (vector char))))
                (f (lookup-key frog-menu--avy-action-map key)))
           (if (functionp f)
               (throw 'done (list f))
             (message "No such candidate, hit `C-g' to quit.")
             (throw 'done 'restart))))))

(defun frog-menu--init-avy-action-map (actions)
  "Initialize `frog-menu--action-map'.

Each action of ACTIONS is bound to a command which returns the
action result. ACTIONS is the argument of `frog-menu-read'."
  (setq frog-menu--avy-action-map (make-sparse-keymap))
  (dolist (action actions)
    (define-key frog-menu--avy-action-map (kbd (car action))
      (lambda () (car (cddr action)))))
  ;; space must not be used by actions
  (define-key frog-menu--avy-action-map "\t" 'frog-menu--complete))

(defun frog-menu-query-with-avy (buffer window actions)
  "Query handler for avy-posframe.

Uses `avy' to query for candidates in BUFFER showing in WINDOW.

ACTIONS is the argument of `frog-menu-read'."
  (let ((candidates (frog-menu--get-avy-candidates
                     buffer window)))
    ;; init map which passes actions info to avy handler
    (frog-menu--init-avy-action-map actions)
    ;; FIXME: These aren't found in my copy of avy!
    (defvar avy-single-candidate-jump) (defvar avy-pre-action)
    (if candidates
        (let* ((avy-keys frog-menu-avy-keys)
               (avy-single-candidate-jump (null actions))
               (avy-handler-function #'frog-menu--posframe-ace-handler)
               (avy-pre-action #'ignore)
               (avy-all-windows nil)
               (avy-style 'at-full)
               (avy-action #'identity)
               (pos (with-selected-window window
                      (avy-process
                       candidates
                       (avy--style-fn avy-style)))))
          (cond ((number-or-marker-p pos)
                 ;; string
                 (with-current-buffer buffer
                   (let* ((start pos)
                          (end (or (next-single-property-change start 'face)
                                   (point-max))))
                     ;; get rid of the padding
                     (replace-regexp-in-string
                      "\\`_ *" "" (buffer-substring start end)))))
                ((eq pos 'frog-menu--complete)
                 ;; switch to completion from `frog-menu-read'
                 pos)
                ((functionp pos)
                 ;; action
                 (funcall pos))))
      (let ((f nil))
        (while (not f)
          (unless (setq f (lookup-key frog-menu--avy-action-map
                                      (vector (read-char))))
            (message "No such action, hit C-g to quit.")))
        (funcall f)))))


;; * Entry point

(defun frog-menu--complete (prompt collection &rest args)
  "PROMPT for `completing-read' COLLECTION.

Remaining ARGS are passed to `completing-read'. PROMPT and
COLLECTION are the arguments from `frog-menu-read'."
  (apply #'completing-read
         ;; make sure prompt is "completing readable"
         (if (string-empty-p prompt)
             ": "
           (replace-regexp-in-string "\\(: ?\\)?\\'" ": " prompt))
         collection args))

(defun frog-menu-completing-read-function (prompt collection predicate &rest _)
  "Can be used as `completing-read-function'.

PROMPT, COLLECTION and PREDICATE are of format as specified by
`completing-read'."
  (let ((strings (frog-menu--collection-to-strings collection predicate)))
    (frog-menu-read prompt strings)))


;;;###autoload
(defun frog-menu-call (cmds &optional prompt)
  "Read a command from CMDS and execute it.

CMDS is of format as specified by `completing-read'
collections. If PROMPT is given it should be a string with prompt
information for the user."
  (let ((cmd (intern-soft (frog-menu-read
                           (or prompt "")
                           (frog-menu--collection-to-strings cmds)))))
    (command-execute cmd)))


(defun frog-menu--collection-to-strings (collection &optional predicate)
  "Return list of strings representing COLLECTION.
COLLECTION and PREDICATE should have the format as specified by
`completing-read'."
  (cond ((functionp collection)
         (let ((cands (funcall collection "" predicate t)))
           (if (stringp (car-safe cands))
               (copy-sequence cands)
             (mapcar #'symbol-name cands))))
        ((listp collection)
         (let ((strings ()))
           (dolist (el collection (nreverse strings))
             (unless (and predicate
                          (funcall predicate el))
               (let ((cand (or (car-safe el) el)))
                 (push (if (symbolp cand)
                           (symbol-name cand)
                         cand)
                       strings))))))
        ((hash-table-p collection)
         (let ((strings ()))
           (maphash
            (lambda (key val)
              (unless (and predicate
                           (funcall predicate key val))
                (push (if (symbolp key)
                           (symbol-name key)
                         key)
                      strings)))
            collection)
           (nreverse strings)))
        ((vectorp collection)
         (let ((strings ()))
           (mapatoms
            (lambda (el)
              (unless (and predicate
                           (funcall predicate el))
                (push (symbol-name el) strings))))
           (nreverse strings)))))


;;;###autoload
(defun frog-menu-read (prompt collection &optional actions)
  "Read from a menu of variable `frog-menu-type'.

PROMPT is a string with prompt information for the user.

COLLECTION is a list from which the user can choose an item. It
can be a list of strings or an alist mapping strings to return
values. Users can switch to `completing-read' from COLLECTION
using the TAB key. For sorting the displayed strings see
`frog-menu-sort-function'.

ACTIONS is an additional list of actions that can be given to let
the user choose an action instead an item from COLLECTION.

Each ACTION is a list of the form:

    (KEY DESCRIPTION RETURN)

Where KEY is a string to be interpreted as spelled-out
keystrokes, using the same format as for `kbd'.

DESCRIPTION is a string to be displayed along with KEY to
describe the action.

RETURN will be the returned value if KEY is pressed."
  (let* ((frog-menu-type (funcall frog-menu-type-function))
         (convf (and collection (consp (car collection))
                     #'car))
         (strings (if convf
                      (mapcar convf collection)
                    collection))
         (strings (if frog-menu-sort-function
                      (sort strings frog-menu-sort-function)
                    strings))
         (buf (frog-menu--init-buffer (get-buffer-create frog-menu--buffer)
                                      prompt
                                      strings
                                      actions))
         (dhandler (cdr (assq frog-menu-type
                              frog-menu-display-handler-alist)))
         (doption (cdr (assq frog-menu-type
                             frog-menu-display-option-alist)))
         (window (funcall dhandler buf doption))
         (qhandler (cdr (assq frog-menu-type
                              frog-menu-query-handler-alist)))
         (cuhandler (cdr (assq frog-menu-type
                               frog-menu-cleanup-handler-alist)))
         (res nil))
    (unwind-protect
        (setq res (funcall qhandler buf window actions))
      (when cuhandler
        (funcall cuhandler buf window)))
    (when (eq res 'frog-menu--complete)
      (setq res (frog-menu--complete prompt strings)))
    (cond ((and (eq convf #'car)
                (stringp res)
                (eq (get-text-property 0 'face res)
                    'frog-menu-candidates-face))
           (cdr (assoc res collection)))
          (t res))))



(provide 'frog-menu)
;;; frog-menu.el ends here




