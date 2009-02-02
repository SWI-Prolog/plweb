:- load_files(plweb,
	      [ silent(true)
	      ]).

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
