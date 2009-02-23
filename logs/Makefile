all::
	rsync -av ec:/home/vnc/prolog/src/plweb/httpd.log .
	pl -s log2clf.pl -q -g main -- -o apache.log httpd.log
	webalizer apache.log
	rsync -aCv report/ ec:/home/vnc/prolog/src/plweb/www/logs
