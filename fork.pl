:- load_files([ library(pldoc/doc_library),
		library(thread_pool),
		http_fork,
		plweb
	      ],
	      [ silent(true)
	      ]).

%:- doc_load_library.

:- debug(http(fork)).

run :-
	forked_server(3040,
		      [ workers(5),
			init(plweb:init_thread_pools)
		      ]).
