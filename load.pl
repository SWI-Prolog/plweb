:- load_files([ library(pldoc/doc_library),
		plweb
	      ],
	      [ silent(true)
	      ]).

%:- doc_load_library.

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
