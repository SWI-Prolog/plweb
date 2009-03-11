% #!/home/jan/bin/pl -q -g main -s

% You can make this program executable by  removing the % from the first
% line and update the path the SWI-Prolog interpreter

% Convert Prolog logs to apache's Combined Log Format
% http://httpd.apache.org/docs/1.3/logs.html#combined

:- use_module(logstat).
:- use_module(library(http/http_header)).
:- use_module(library(main)).

:-dynamic
	outfile/1,
	logfile/1.

main([]) :- !,
	format(user_error, 'Usage: log2clf [-o out] file ...~n', []),
	halt(1).
main(Argv) :-
	retractall(outfile(_)),
	retractall(logfile(_)),
        process_argv(Argv),
	(   outfile(Out)
	->  write_clf(Out)
	;   write_records(current_output)
	).

process_argv([]).
process_argv(['-o', OutFile|T]) :-
	assert(outfile(OutFile)),!,
        process_argv(T).
	
process_argv([F|T]) :-
	assert(logfile(F)),
	read_log(F),!,
        process_argv(T).

writerecord(Out, LogRecord) :-
	memberchk(ip(IP), LogRecord),
	memberchk(time(Time), LogRecord),
	memberchk(path(Path), LogRecord),	
	memberchk(referer(Referer), LogRecord),	
	memberchk(result(Result), LogRecord),
	memberchk(extra(Extra), LogRecord),
	(memberchk(user_agent(Agent), Extra) -> true; Agent='-'),
	memberchk(http_version(Http1-Http2), Extra),
	format_time(atom(TimeStamp), "[%d/%b/%Y:%H:%M:%S %z]",Time),
	result_code(Extra, Result, ResultCode),
	(memberchk(bytes(Bytes), Extra) -> true; Bytes='-'),
	format(Out, '~w - - ~w "GET ~w HTTP/~w.~w" ~w ~w "~w" "~w"~n', 
	       [IP, TimeStamp, Path, Http1, Http2, ResultCode, Bytes, Referer, Agent]),
	!.

writerecord(_, LogRecord) :-
	format(user_error, "Could not convert log record ~w~n", [LogRecord]).
	
write_clf(Out):-
	open(Out, write, OutS, [encoding(utf8)]),
	call_cleanup(write_records(OutS), close(OutS)).

%%	write_records(+Stream) is det.
%
%	Write the records. If we have multiple files, we must sort them.
%	Possibly this should be an option. It  was designed to deal with
%	multiple files from a load-balancer, but it could of course also
%	be used with multiple files from a single server.

write_records(OutS) :-
	Queries = [ip(_), path(_), referer(_), result(_), extra(_)],	
	findall(F, logfile(F), Fs), 
	length(Fs, NFiles),
	(   NFiles =< 1
	->  Params = [time(Time)|Queries],
	    forall(logrecord(Params),
		   writerecord(OutS,Params))
	;   findall(Time-logrecord([time(Time)|Queries]),
		    logrecord([time(Time)|Queries]),
		    Logrecords),
	    keysort(Logrecords, Sorted),
	    forall(member(_-logrecord(Params),Sorted),
		   writerecord(OutS,Params))
	).


result_code(Extra, _, Code) :-
	memberchk(code(Code), Extra), !.
result_code(_, Status, Code) :-
	status_to_code(Status, Code).

status_to_code(true,200)                :- !.
status_to_code(file(_,_), 200)          :- !.
status_to_code(tmp_file(_,_), 200)          :- !.
status_to_code(moved(_), 301)           :- !.
status_to_code(not_modified, 304)	     :- !.
status_to_code(moved_temporary(_), 307) :- !.
status_to_code(error(404,_), 404)       :- !.
status_to_code(error(_R), 400)          :- !.
	% format('400 error for result ~w~n', R).
status_to_code(unavailable(_), 503)     :- !.
status_to_code(no_reply, 500)     :- !.
status_to_code(R,-) :-
	format('Error: No map for result ~w~n', [R]),!.
	
