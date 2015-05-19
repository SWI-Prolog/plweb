set breakpoint pending on
break trap_gdb
break sysError
break __asan_report_error
set breakpoint pending off

handle SIGPIPE noprint nostop pass
handle SIGUSR1 noprint nostop pass
handle SIGUSR2 noprint nostop pass
# Boehm-GC signals
handle SIGPWR  noprint nostop pass
handle SIGXCPU noprint nostop pass
# handle SIGSEGV noprint nostop pass
set print thread-events off

define pl
  dont-repeat
  run -f test.pl -O -F none
end

define qpl
  dont-repeat
  run -q -f ../src/test.pl -O -F none
end

define test
  dont-repeat
  run -q -f ../src/test.pl -O -F none -g test,halt -t 'halt(1)'
end

define boot
  dont-repeat
  run -O -o swipl.prc -b ../boot/init.pl
end

define full
  dont-repeat
  set environment SWI_HOME_DIR=/home/janw/lib/swipl
  run
end

define ef
  set environment LD_PRELOAD=libefence.so.0.0
end

define dleak
  set environment LD_PRELOAD=/home/janw/src/dleak/dleak.so
end

define leak
  set environment GC_FIND_LEAK=t
end

define man
  dont-repeat
  set environment SWI_HOME_DIR=/home/janw/lib/swipl
  run -f none -O -g "qcompile('pce_manual')" -t halt -- -nopce
end

define safe-bt
  call PL_backtrace(20, 0x1)
end

define prolog-bt
  call PL_backtrace(20, 0)
end

