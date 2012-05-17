YUI_VERSION = 3.5.1

all: happstack-yui.cabal

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
