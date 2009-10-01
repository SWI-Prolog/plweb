all:	sync convert webalize upload

sync::
	rsync -av ec:/home/vnc/prolog/src/plweb/httpd.log .

convert::
	pl -G1g -s log2clf.pl -q -g main -- -o apache.log httpd.log

webalize::
	webalizer apache.log

upload::
	rsync -aCv --delete report/ ec:/home/vnc/prolog/src/plweb/www/logs
