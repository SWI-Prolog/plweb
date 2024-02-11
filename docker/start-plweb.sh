#!/bin/bash
#
# Start script for the PlWeb docker
#
# This script is started in /srv/plweb.

start=--no-fork
ssl=
scheme=http
udaemon=daemon
uconfig=root
config_run=no

date "+%s" > /var/run/epoch

usage()
{ echo "Usage: docker run [docker options] swish [swish options]"
  echo "swish options:"
  echo "  --help                 Display this message"
  echo "  --bash		 Just run bash in the container"
  echo "  --auth=type            Configure authentication>:"
  echo "         always          Force HTTP authentication"
  echo "         social          Allow HTTP and oauth2 authentication"
  echo "         anonymous       No authentication"
  echo "  --add-user		 Add a new user"
  echo "  --http		 Create an HTTP server"
  echo "  --https		 Create an HTTPS server"
  echo "  --CN=host		 Hostname for certificate"
  echo "  --O=organization	 Organization for certificate"
  echo "  --C=country		 Country for certificate"
  echo "  --run			 Start SWISH after config options"
  echo "  --list-config		 List configuration files"
  echo "  --add-config file ...	 Add a configuration file"
  echo ""
  echo "--add-config should be the last option if used."
}

# `mkuser file user` creates user with the uid and gid of file.

mkuser()
{ f="$1"
  u="$2"

  groupadd "$(ls -nd "$f" | awk '{printf "-g %s\n",$4 }')" -o $u
  useradd  "$(ls -nd "$f" | awk '{printf "-u %s\n",$3 }')" -g $u -o $u
}

# If there is a data directory, reuse it and set our user to be the
# native user of this directory.

if [ -d /srv/plweb/data ]; then
  mkuser /srv/plweb/data plweb
  udaemon=plweb
else
  mkdir /srv/plweb/data
  chown $udaemon.$udaemon /srv/plweb/data
fi

# Allow the daemon to get the git version
mkdir -p /home/$udaemon
chown $udaemon /home/$udaemon
# su -c "git config --global --add safe.directory /swish" $udaemon

if [ -t 0 ] ; then
  start=--interactive
fi

did_config=no

while [ ! -z "$1" ]; do
  case "$1" in
    --bash)		su $udaemon -c /bin/bash
			did_config=yes
			shift
			;;
    --help)		usage
			exit 0
			;;
    *)			usage
			exit 1
			;;
  esac
done

if [ $did_config = yes -a $config_run = no ]; then
  exit 0
fi

## Make the server stop on signals sent from the health.sh.  Process
## 1 only accepts signals for which there is an installed signal
## handler.  We cannot install a signal handler for SIGKILL and
## therefore forcefully killing that even works in the case of
## deadlocks does not work.   We run the server in another pid
## to work around this issue.

stop()
{ echo "signal = $1; child = $child_pid"

  kill -s $1 $child_pid
  timeout 10 tail --pid=$child_pid -f /dev/null
  if [ $? == 124 ]; then
      echo "Gracefull termination failed.  Killing"
      kill -s KILL $child_pid
  fi

  exit 1
}

hangup()
{ echo "child = $child_pid"
  kill -s HUP $child_pid
}

trap "stop TERM" SIGTERM
trap "stop QUIT" SIGQUIT
trap "hangup" SIGHUP

export HOME=/home/$udaemon 

git config --global --add safe.directory '*'
git config --global user.email "wiki@swi-prolog.org"
git config --global user.name "Wiki editor"

swipl ${PLWEB_HOME}/daemon.pl --port=3400 --user=$udaemon $start &
child_pid=$!

stat=129
while [ $stat = 129 ]; do
  wait -f $child_pid
  stat=$?
done
