:- doc_collect(true).
:- load_files([ library(pldoc/doc_library),
		library(debug),
		library(settings),
		library(option),
		http_fork,
		plweb,
		wiki_edit
	      ],
	      [ silent(true)
	      ]).

:- doc_load_library.

:- debug(http(fork)).

run :-
	run([]).

run(Options) :-
	load_settings('plweb.conf'),
	setting(http:port, Port),
	setting(http:workers, Workers),
	merge_options(Options,
		      [ port(Port),
			workers(Workers)
		      ], HTTPOptions),
	option(port(Port), HTTPOptions),
	forked_server(Port,
		      [ HTTPOptions
		      ]).
