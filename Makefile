TOP       := $(dir $(lastword $(MAKEFILE_LIST)))

ALL_EMACS_VERS := $(shell compgen -c emacs- | grep -oP '(?<=emacs-)([0-9]|\.)+' | sort | uniq)
EMACS          ?= emacs

LOAD_PATH := -L $(TOP)
BATCH     := $(EMACS) -Q --batch $(LOAD_PATH)

ELS   := srt.el
ELCS  := $(ELS:.el=.elc)

all: git-hook build

git-hook:
# cp git hooks to .git/hooks
	cp -a git-hooks/* .git/hooks/

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	-@$(BATCH) -f batch-byte-compile $<

test:
# If byte compile for specific emacs,
# set EMACS such as `EMACS=26.1 make`.
	make clean
	$(BATCH) -l srt-tests.el -f srt-run-tests

localtest:
# Clean all of .elc, compile .el, and run test.

	$(call ECHO_MAGENTA, "test by emacs-22.1")
	make clean
	EMACS=emacs-22.1 make test

	@echo "\n"
	$(call ECHO_MAGENTA, "test by emacs-24.5")
	make clean
	EMACS=emacs-24.5 make test

	@echo "\n"
	$(call ECHO_MAGENTA, "test by emacs-26.1")
	make clean
	EMACS=emacs-26.1 make test

	@echo "\n"
	$(call ECHO_CYAN, "localtest completed!!")
	@echo "\n"

debug-localtest:
# Clean all of .elc, compile .el, and run test.
# don't stop on error, run test on all of emacs.
# $status = array ${#a[@]}

	for ver in $(ALL_EMACS_VERS); do \
		$(COLOR_MAGENTA); \
		echo "===  test by emacs-$${ver}...  ==="; \
		$(COLOR_DEFAULT); \
		\
		EMACS=emacs-$${ver} make test; \
	done
	@echo "\n"
	$(call ECHO_CYAN, "debug-localtest completed!!")
	@echo "\n"

clean:
	-find . -type f -name "*.elc" | xargs rm

include Makefunc.mk

