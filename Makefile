YUI_VERSION = 3.5.1

all: bundle

yui_$(YUI_VERSION).zip:
	wget http://yui.zenfs.com/releases/yui3/$@

bundle: yui_$(YUI_VERSION).zip
	unzip $< 'yui/build/*'
	mv yui/build bundle
	rmdir yui
