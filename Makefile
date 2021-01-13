VERSION := 0.22.1

all: pandoc-sidenote-$(VERSION).zip

.PHONY: stack
stack:
	stack build

pandoc-sidenote-%.zip: stack
	find .stack-work/install -name pandoc-sidenote -type f \
	     -exec zip -j $@ '{}' ';'

# Assumes that a tag has already been created and pushed
.PHONY: release
release: pandoc-sidenote-$(VERSION).zip
	hub release create -a $< $(VERSION)
