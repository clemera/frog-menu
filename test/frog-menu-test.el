(require 'ert)
(require 'frog-menu)

(ert-deftest frog-menu-test-stub ()
  (should
   (eq frog-menu-type-function #'frog-menu-type)))
