#!/bin/bash
#
# Docker HEALTHCHECK for the plweb server, run every 30 seconds (see
# Dockerfile).  A single slow or failed request does not kill the
# server: under heavy load /health may take several seconds while the
# server is otherwise fine.  We only restart after $max_failures
# consecutive failed checks.

log=health.log
failures_file=/var/run/health.failures
max_failures=3

# Keep a log over several runs, but not unbounded
if [ -f $log ] && [ $(stat -c %s $log) -gt 100000 ]; then
  mv $log $log.old
fi
exec &>> $log

check()
{ curl --fail -s -o /dev/null --retry 2 --max-time 10 \
       http://localhost:3400/health
}

stop()
{ pid=1
  echo "$(date -u '+%F %T') Health check failed $failures times.  Killing plweb with SIGTERM"
  kill -s TERM $pid
  timeout 10 tail --pid=$pid -f /dev/null
  if [ $? == 124 ]; then
      echo "Gracefull termination failed.  Trying QUIT"
      kill -s QUIT $pid
      timeout 10 tail --pid=$pid -f /dev/null
      if [ $? == 124 ]; then
	   echo "QUIT failed.  Trying KILL"
	   kill -s KILL $pid
      fi
  fi
  echo "Done"
}

starting()
{ if [ -f /var/run/epoch ]; then
      epoch=$(cat /var/run/epoch)
      running=$(($(date "+%s") - $epoch))
      [ $running -lt 60 ] || return 1
  fi
  echo "$(date -u '+%F %T') Starting, so not killing"
  return 0
}

if check; then
  rm -f $failures_file
  exit 0
fi

starting && exit 1

failures=$(( $(cat $failures_file 2>/dev/null || echo 0) + 1 ))
echo $failures > $failures_file
echo "$(date -u '+%F %T') Health check failed ($failures/$max_failures)"

if [ $failures -ge $max_failures ]; then
  rm -f $failures_file
  stop
fi
exit 1
