#!/bin/sh
### BEGIN INIT INFO
# Provides:          swipl-website
# Required-Start:    $local_fs $remote_fs $network $syslog $named
# Required-Stop:     $local_fs $remote_fs $network $syslog $named
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# X-Interactive:     true
# Short-Description: Start/stop SWI-Prolog web server
### END INIT INFO

# Installation
#
#   1. Copy this file to /etc/init.d/<server>
#   2. Edit the Configuration section below
#   3. Run
#	 % update-rc.d <server> defaults
#	 % /etc/init.d/<server> start

# Configuration section
#
SWIPL=/home/janw/bin/swipl
DIR=/home/janw/src/plweb
SCRIPT=daemon.pl
USER=www
PORT=80
DAEMONARGS=

# Uncomment to only listen for connections from localhost
# DAEMONARGS="$DAEMONARGS --ip=localhost"

HTTPID=swipl-httpd-$PORT
PIDFILE=/var/run/$HTTPID.pid
SYSLOG=$HTTPID

# End of normal configuration

. /lib/lsb/init-functions
test -f /etc/default/rcS && . /etc/default/rcS

DAEMONARGS="$DAEMONARGS --port=$PORT --user=$USER --fork"
if [ ! -z "$SYSLOG" ]; then DAEMONARGS="$DAEMONARGS --syslog=$SYSLOG"; fi
if [ ! -z "$PIDFILE" ]; then DAEMONARGS="$DAEMONARGS --pidfile=$PIDFILE"; fi

pidofserver()
{ if [ -f "$PIDFILE" ]; then
    cat "$PIDFILE"
  else
    ps aux | grep "[0-9] *$SWIPL.*--port=$PORT" 2>/dev/null | awk '{print $2}'
  fi
}

running()
{ if [ -z "$1" ]; then return 1; fi

  if kill -0 $1 2> /dev/null; then
    return 0
  else
    return 1
  fi
}

waitserver()
{ i=0;

  while running $1; do
    if [ $i = '60' ]; then
      return 1;
    else
      if [ $i = '0' ]; then
        echo -n " ... waiting "
      else
        echo -n "."
      fi
      i=$(($i+1))
      sleep 1
    fi
  done
}

case $1 in
        start)
	  log_daemon_msg "Starting web server" "$HTTPID"
	  if (cd $DIR && $SWIPL -q -f $SCRIPT -- $DAEMONARGS); then
	    log_end_msg 0
	  else
	    log_end_msg 1
	  fi
        ;;
	stop)
	  log_daemon_msg "Stopping web server" "$HTTPID"
	  PID=$(pidofserver)
	  kill $PID
	  if waitserver $PID; then
	    log_end_msg 0
	  else
	    kill -9 $PID
	    waitserver $PID
	  fi
	;;
	reload)
	  PID=$(pidofserver)
	  kill -HUP $PID
	;;
	restart)
	  $0 stop && $0 start
	;;
	status)
	  PID=$(pidofserver)
	  if running "$PID"; then
	    echo "SWI-Prolog HTTP server is running (pid $PID)."
	    exit 0
	  else
	    echo "SWI-Prolog HTTP server is NOT running."
	    if [ -e $PIDFILE ]; then
	      exit 1
	    else
	      exit 3
	    fi
	  fi
	;;
	*)
	log_success_msg "Usage: /etc/init.d/swipl-httpd {start|stop|restart|status}"
	exit 1
esac
