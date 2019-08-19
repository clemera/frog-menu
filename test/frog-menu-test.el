(require 'ert)
(require 'frog-menu)


;; TODO: test grid creation as non interactive test:
;; all items in it? correctly ordered? respecting dimensions?


;; tests for interactive usage
;; load tests and run ert
(unless noninteractive
  (require 'with-simulated-input)
  (ert-deftest frog-menu-test-stub ()
    (should (string= (with-simulated-input "a"
                       (frog-menu-read "Check: " '("this" "that" "more")))
                     "this")))
  (ert-deftest frog-menu-test-stub ()
    (should (string= (with-simulated-input "s"
                       (frog-menu-read "Check: " '("this" "that" "more")))
                     "that")))
  (when load-file-name
    (ert-run-tests-batch nil)))







