VERSION := 0.19.0

all: pandoc-sidenote-$(VERSION).zip

.PHONY: stack
stack:
	stack build

pandoc-sidenote-%.zip: stack
	cd .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/ && \
	zip -r pandoc-sidenote pandoc-sidenote && \
	cd ../../../../../ && \
	mv .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/pandoc-sidenote.zip $@

