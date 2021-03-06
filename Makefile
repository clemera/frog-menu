EMACS ?= emacs

package_files := $(wildcard *.el)
test_files := $(wildcard test/*.el)
package_lint  := ~/package-lint/package-lint.el

.PHONY: all
all: compile checkdoc lint test itest

.PHONY: compile
compile:
	@for file in $(package_files); do \
		echo "[compile] $$file" ;\
		$(EMACS) -Q --batch -L . -L stub \
		--eval "(setq byte-compile-error-on-warn t)"\
		-f batch-byte-compile $$file;\
	done

.PHONY: checkdoc
checkdoc:
	@for file in $(package_files); do \
		echo "[checkdoc] $$file" ;\
		$(EMACS) -Q --batch \
			--eval "(setq sentence-end-double-space nil)" \
			--eval "(checkdoc-file \"$$file\")" \
			--eval "(when (get-buffer \"*Warnings*\") (kill-emacs 1))" ;\
	done

.PHONY: lint
lint:
	@for file in $(package_files); do \
		echo "[package-lint] $$file" ;\
		$(EMACS) -Q --batch \
			-l $(package_lint) \
			--eval "(defalias 'package-lint--check-packages-installable #'ignore)" \
			-f package-lint-batch-and-exit $$file ;\
	done

.PHONY: test
test:
	@for file in $(test_files); do \
		echo "[ert-test] $$file" ;\
		$(EMACS) -Q --batch -L . -L stub \
			-l $$file \
			-f ert-run-tests-batch-and-exit ;\
	done

.PHONY: itest
itest:
	@if emacsclient -a false -e 't' 1>/dev/null 2>/dev/null; then \
		echo "[interactive-test]" ;\
		emacsclient --eval "(load-file \"test/frog-menu-test.el\")" ;\
	fi

.PHONY: clean
clean:
	@echo "[clean]" *.elc
	@rm -f *.elc
