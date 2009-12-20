:- doc_collect(true).
:- load_files([ library(pldoc/doc_library),
		library(thread_pool),
		plweb
	      ],
	      [ silent(true)
	      ]).

:- doc_load_library.
:- send(@pce, catch_error_signals, @off).

%%	show_fd
%
%	Show open file descriptors.  Sanity-check   that  works  only on
%	Linux systems.

show_fd :-
        current_prolog_flag(pid, Pid),
        format(string(Cmd),
               '/bin/sh -c "(cd /proc/~w/fd && ls -l | grep socket)"',
               [Pid]),
        shell(Cmd).

show_pools :-
	format('~`-t~52|~n'),
	format('~w~t~20|~t~w~8+~t~w~8+~t~w~8+~t~w~8+~n',
	       [ 'Pool name', 'Running', 'Size', 'Waiting', 'Backlog' ]),
	format('~`-t~52|~n'),
	forall(current_thread_pool(Pool), show_pool(Pool)),
	format('~`-t~52|~n').

show_pool(Pool) :-
	findall(P, thread_pool_property(Pool, P), List),
	memberchk(size(Size), List),
	memberchk(running(Running), List),
	memberchk(backlog(Waiting), List),
	memberchk(options(Options), List),
	option(backlog(MaxBackLog), Options, infinite),
	format('~w~t~20|~t~D  ~8+~t~D ~8+~t~D  ~8+~t~w  ~8+~n',
	       [Pool, Running, Size, Waiting, MaxBackLog]).

stop :-
	halt(42).
