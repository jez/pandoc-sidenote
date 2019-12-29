VERSION := 0.20.0

all: pandoc-sidenote-$(VERSION).zip

.PHONY: stack
stack:
	stack build

pandoc-sidenote-%.zip: stack
	find .stack-work/install -name pandoc-sidenote -type f \
	     -exec zip -j $@ '{}' ';'

