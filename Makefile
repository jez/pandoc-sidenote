VERSION := 0.23.0

all: pandoc-sidenote-$(VERSION).zip

.PHONY: stack
stack:
	stack build

pandoc-sidenote-%.zip: stack
	find .stack-work/install/x86_64-osx -name pandoc-sidenote -type f \
	     -exec zip -j $@ '{}' ';'

# Assumes that a tag has already been created and pushed
.PHONY: release
release: pandoc-sidenote-$(VERSION).zip
	hub release create -a $< $(VERSION)
