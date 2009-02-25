/*  File:    http_cgi.pl
    Author:  Jan Wielemaker
    Created: Jan 28 2009
    Purpose: Run CGI scripts
*/

:- module(http_cgi,
	  [ http_run_cgi/2		% +Script, +Request
	  ]).
:- use_module(library(process)).
:- use_module(library(socket)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(http/http_dispatch)).

/** <module> Run CGI scripts from the SWI-Prolog web-server

Run external scripts.  This module provides two interfaces:

	* http_run_cgi/2 can be used to call a CGI script
	located exernally
	
	* Setup a path =cgi_bin= for absolute_file_name/3.  If
	this is present, calls to /cgi-bin/... are translated into
	calling the script.

@tbd complete environment translation.  See env/3.
@tbd testing.  Notably for POST and PUT commands.
@tbd the Windows version of library(process) does not yet
     support the =env= option.
@see http://hoohoo.ncsa.uiuc.edu/cgi/env.html
*/

:- multifile
	environment/2.

:- http_handler(root('cgi-bin'), run_script, [ prefix ]).

run_script(Request) :-
	select(path_info(PathInfo), Request, Request1),
	ensure_no_leading_slash(PathInfo, Relative),
	path_info(Relative, Script, Request1, Request2),
	absolute_file_name(cgi_bin(Script), ScriptFileName,
			   [ access(execute)
			   ]),
	http_run_cgi(ScriptFileName, Request2).

			   
ensure_no_leading_slash(Abs, Rel) :-
	atom_concat(/, Rel, Abs), !.
ensure_no_leading_slash(Rel, Rel).

path_info(RelPath, Script, Req, [path_info(Info)|Req]) :-
	sub_atom(RelPath, Before, _, After, /), !,
	sub_atom(RelPath, 0, Before, _, Script),
	sub_atom(RelPath, _, After, 0, Info).
path_info(Script, Script, Request, Request).


%%	http_run_cgi(+Script, +Request) is det.
%
%	Execute the given CGI script.

http_run_cgi(Script, Request) :-
	input_handle(Request, ScriptInput),
	findall(Name=Value,
		env(Name,
		    [ script_file_name(Script)
		    | Request
		    ], Value),
		Env),
	process_create(Script, [],
		       [ stdin(ScriptInput),
			 stdout(pipe(CGI)),
			 stderr(std),
			 env(Env),
			 process(PID)
		       ]),
	setup_input(ScriptInput, Request),
	debug(http(cgi), 'Waiting for CGI data ...', []),
	call_cleanup(copy_stream_data(CGI, current_output),
		     (	 process_wait(PID, Status),
			 close(CGI),
			 debug(http(cgi), '~w ended with status ~w',
			       [Script, Status])
		     )).

input_handle(Request, pipe(_)) :-
	memberchk(method(Method), Request),
	method_has_data(Method), !.
input_handle(_, std).

method_has_data(post).
method_has_data(put).

setup_input(std, _).
setup_input(pipe(Stream), Request) :-
	memberchk(input(In), Request),
	(   memberchk(content_length(Len), Request)
	->  true
	;   Len = unknown
	),
	thread_create(copy_post_data(In, Stream, Len), _,
		      [ detached(true),
			local(1000),
			global(1000),
			trail(1000)
		      ]).

copy_post_data(In, Script, unknown) :-
	catch(copy_stream_data(In, Script), _, true),
	close(Script, [force(true)]).
copy_post_data(In, Script, Len) :-
	catch(copy_stream_data(In, Script, Len), _, true),
	close(Script, [force(true)]).
		  

%%	env(?Name, +Request, -Value) is nondet.
%
%	Enumerate the environment variables to be   passed  to the child
%	process.

env('SERVER_SOFTWARE', _, Version) :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
	format(atom(Version), 'SWI-Prolog/~w.~w.~w', [Major, Minor, Patch]).
env('SERVER_NAME', Request, Server) :-
	(   memberchk(x_forwarded_host(Server), Request)
	->  true
	;   memberchk(host(Server), Request)
	->  true
	;   gethostname(Server)
	).
env('GATEWAY_INTERFACE', _, 'CGI/1.1').
env('SERVER_PROTOCOL', Request, Protocol) :-
	memberchk(http(Major-Minor), Request),
	format(atom(Protocol), 'HTTP/~w.~w', [Major, Minor]).
env('SERVER_PORT', Request, Port) :-
	(   memberchk(port(Port), Request),
	    \+ memberchk(x_forwarded_host(_), Request)
	->  true
	;   Port = 80
	).
env('REQUEST_METHOD', Request, Method) :-
	memberchk(method(LwrCase), Request),
	upcase_atom(LwrCase, Method).
env('PATH_INFO', Request, PathInfo) :-
	memberchk(path_info(PathInfo), Request).
env('PATH_TRANSLATED', _, _) :- fail.
env('SCRIPT_NAME', _, _) :- fail.
env('SCRIPT_FILENAME', Request, ScriptFilename) :-
	memberchk(script_file_name(ScriptFilename), Request).
env('QUERY_STRING', Request, QString) :-
	memberchk(search(Search), Request),
	parse_url_search(QList, Search),
	string_to_list(QString, QList).
env('REMOTE_HOST', _, _) :- fail.
env('REMOTE_ADDR', _, _) :- fail.
env('AUTH_TYPE', _, _) :- fail.
env('REMOTE_USER', Request, User) :-
	memberchk(user(User), Request).
env('REMOTE_IDENT', _, _) :- fail.
env('CONTENT_TYPE', Request, ContentType) :-
	memberchk(content_type(ContentType), Request).
env('CONTENT_LENGTH', Request, ContentLength) :-
	memberchk(content_length(ContentLength), Request).
env('HTTP_ACCEPT', _, _) :- fail.
env('HTTP_USER_AGENT', Request, Agent) :-
	memberchk(user_agent(Agent), Request).
env(Name, _, Value) :-
	environment(Name, Value).

