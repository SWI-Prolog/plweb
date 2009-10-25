set breakpoint pending on
break trap_gdb
break sysError
set breakpoint pending off

handle SIGPIPE noprint nostop pass
handle SIGHUP  noprint nostop pass
handle SIGUSR2 noprint nostop pass
set print thread-events off

define pl
  dont-repeat
  run -f test.pl -G2M -L2M -O -F none
end

define qpl
  dont-repeat
  run -q -f ../src/test.pl -G2M -L2M -O -F none
end

define test
  dont-repeat
  run -q -f ../src/test.pl -G2M -L2M -O -F none -g test,halt -t 'halt(1)'
end

define ef
  set environment LD_PRELOAD=libefence.so.0.0
end
