HACKAGE = hackline


.PHONY: build
build:
	cabal v2-build hackline

.PHONY: run
run:
	cabal v2-run $(HACKAGE)

.PHONY: configure
configure:
	cabal v2-configure --enable-tests

.PHONY: test
test:
	cabal v2-test

.PHONY: install
install:
	-rm -f ~/.cabal/bin/$(HACKAGE)
	cabal v2-install $(HACKAGE)

.PHONY: clean
clean::
	-cabal v2-clean
