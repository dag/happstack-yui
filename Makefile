YUI_VERSION = 3.5.1
PATH := $(PWD)/cabal-dev/bin:$(PATH)

all: install

yui_$(YUI_VERSION).zip:
	wget http://yui.zenfs.com/releases/yui3/$@

bundle: yui_$(YUI_VERSION).zip
	unzip $< 'yui/build/*'
	mv yui/build bundle
	rmdir yui
	rm -rf bundle/build

bundle.h: bundle
	find bundle -type f > $@

happstack-yui.cabal: happstack-yui.cabal.in bundle.h
	cpp -P -traditional-cpp -DYUI_VERSION=$(YUI_VERSION) '-DYUI_VERSION_STR="$(YUI_VERSION)"' $< $@

.PHONY: build
build: happstack-yui.cabal
	cabal-dev build

.PHONY: install
install: happstack-yui.cabal
	cabal-dev install

.PHONY: sdist
sdist: happstack-yui.cabal
	cabal sdist

.PHONY: docs
docs: happstack-yui.cabal
	cabal haddock

.PHONY: demo
demo: install
	cabal-dev install ./demo
	cabal-dev/bin/happstack-yui-demo
