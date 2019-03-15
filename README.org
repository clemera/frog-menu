* Description

This package lets you quickly pick strings from ad hoc menus. Just like a frog
would catch a fly. The menu is built "on the fly" from a collection of
strings. It's presented to the user to choose one of them by pressing a single
key. One example where this kind of menu is useful are spelling correction
suggestions:

[[./images/spellcheck.png]]

The user can specify a prompt and additional action keys as you can see in the
bottom of the menu. Usage in the terminal is also supported:

[[./images/spellcheck2.png]]


Inspired by [[https://github.com/mrkkrp/ace-popup-menu][ace-popup-menu]].

* Example

To invoke the menu users can call =frog-menu-read=. How items are displayed
and choosen depends on =frog-menu-type=. For graphical displays the type
=avy-posframe= uses [[https://github.com/abo-abo/avy][avy]] and [[https://github.com/tumashu/posframe][posframe]]. In terminals the type =avy-side-window=
is used. The implemented handler functions can be used as reference if you
want to define your own =frog-menu-type=.

Here is an example how you would invoke a frog menu:

#+begin_src elisp
(frog-menu-read "Choose a string"
                '("a" "list" "of strings"))
#+end_src

It is also possible to define additional action keys (as shown in the
screenshot). Here is an example how you could use =frog-menu-read= to
implement a [[https://github.com/d12frosted/flyspell-correct][flyspell-correct-interface]]:

#+begin_src elisp
(require 'flyspell-correct)

(defun frog-menu-flyspell-correct (candidates word)
  "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
  (let* ((corrects (if flyspell-sort-corrections
                       (sort candidates 'string<)
                     candidates))
         (actions `(("C-s" "Save word"         (save    . ,word))
                    ("C-a" "Accept (session)"  (session . ,word))
                    ("C-b" "Accept (buffer)"   (buffer  . ,word))
                    ("C-c" "Skip"              (skip    . ,word))))
         (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                   ispell-dictionary
                                                   "default")))
         (res      (frog-menu-read prompt corrects actions)))
    (unless res
      (error "Quit"))
    res))

(setq flyspell-correct-interface #'frog-menu-flyspell-correct)
#+end_src

Afterwards calling =M-x flyspell-correct-wrapper= will prompt you with a
=frog-menu=.

