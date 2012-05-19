YUI_VERSION = 3.5.1

all: install

yui_$(YUI_VERSION).zip:
	wget http://yui.zenfs.com/releases/yui3/$@

bundle: yui_$(YUI_VERSION).zip
	unzip $< 'yui/build/*'
	mv yui/build bundle
	rmdir yui

bundle.h: bundle
	find bundle -type f > $@

happstack-yui.cabal: happstack-yui.cabal.in bundle.h
	cpp -P -traditional-cpp $< $@

.PHONY: build
build: happstack-yui.cabal
	cabal-dev build

.PHONY: install
install: happstack-yui.cabal
	cabal-dev install

sdist: happstack-yui.cabal
	cabal sdist

.PHONY: demo
demo: install
	PATH=$(PWD)/cabal-dev/bin:$(PATH) cabal-dev install ./demo
	cabal-dev/bin/happstack-yui-demo
