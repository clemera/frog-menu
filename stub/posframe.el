
(require 'cl-lib)

(defvar posframe--frame nil)

(cl-defun posframe-show (posframe-buffer
                         &key
                         string
                         position
                         poshandler
                         width
                         height
                         min-width
                         min-height
                         x-pixel-offset
                         y-pixel-offset
                         left-fringe
                         right-fringe
                         internal-border-width
                         internal-border-color
                         font
                         foreground-color
                         background-color
                         respect-header-line
                         respect-mode-line
                         initialize
                         no-properties
                         keep-ratio
                         override-parameters
                         timeout
                         refresh
                         &allow-other-keys))

(defun posframe-hide (posframe-buffer))
(defun posframe-poshandler-point-bottom-left-corner (info &optional font-height))

(provide 'posframe)
