/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2011, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_cgi,
	  [ http_run_cgi/3,		% +Script, +Options, +Request
	    http_cgi_handler/2		% +Alias, +Request
	  ]).
:- use_module(library(process)).
:- use_module(library(socket)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_stream)).

:- predicate_options(http_run_cgi/3, 2,
		     [ argv(list),
		       transfer_encoding(atom),
		       buffer(oneof([full,line,none]))
		     ]).

/** <module> Run CGI scripts from the SWI-Prolog web-server

Run CGI scripts.  This module provides two interfaces:

	* http_run_cgi/2 can be used to call a CGI script
	located exernally.  This is typically used for an
	individual script used to extend the server functionality.

	* Setup a path =cgi_bin= for absolute_file_name/3.  If
	this path is present, calls to /cgi-bin/... are translated into
	calling the script.

@tbd complete environment translation.  See env/3.
@tbd testing.  Notably for POST and PUT commands.
@see http://hoohoo.ncsa.uiuc.edu/cgi/env.html
*/

:- multifile
	environment/2.

:- http_handler(root('cgi-bin'), http_cgi_handler(cgi_bin),
		[prefix, spawn([])]).

%%	http_cgi_handler(+Alias, +Request)
%
%	Locate a CGI script  in  the   file-search-path  Alias  from the
%	=path_info=  in  Request   and   execute    the   script   using
%	http_run_cgi/2. This library installs one handler using:
%
%	  ==
%	  :- http_handler(root('cgi-bin'), http_run_cgi(cgi_bin),
%			  [prefix, spawn([])]).
%	  ==

http_cgi_handler(Alias, Request) :-
	select(path_info(PathInfo), Request, Request1),
	ensure_no_leading_slash(PathInfo, Relative),
	path_info(Relative, Script, Request1, Request2),
	Spec =.. [Alias, Script],
	absolute_file_name(Spec, ScriptFileName,
			   [ access(execute)
			   ]),
	http_run_cgi(ScriptFileName, [], Request2).


ensure_no_leading_slash(Abs, Rel) :-
	atom_concat(/, Rel, Abs), !.
ensure_no_leading_slash(Rel, Rel).

ensure_leading_slash(PathInfo, Abs) :-
	(   sub_atom(PathInfo, 0, _, _, /)
	->  Abs = PathInfo
	;   atom_concat(/, PathInfo, Abs)
	).

path_info(RelPath, Script, Req, [path_info(Info)|Req]) :-
	sub_atom(RelPath, Before, _, After, /), !,
	sub_atom(RelPath, 0, Before, _, Script),
	sub_atom(RelPath, _, After, 0, Info).
path_info(Script, Script, Request, Request).


%%	http_run_cgi(+Script, +Options, +Request) is det.
%
%	Execute the given CGI script.  Options processed:
%
%	  * argv(+List)
%	  Argument vector to give to the CGI script.  Defaults to
%	  no arguments.
%	  * transfer_encoding(Encoding)
%	  Emit a =|Transfer-encoding|= header
%	  * buffer(+Buffer)
%	  Set buffering of the CGI output stream.  Typically used
%	  together with transfer_encoding(chunked).
%
%	@param	Script specifies the location of the script as a
%		specification for absolute_file_name/3.
%	@param	Request holds the current HTTP request passed from
%		the HTTP handler.

http_run_cgi(ScriptSpec, Options, Request) :-
	option(argv(Argv), Options, []),
	absolute_file_name(ScriptSpec, Script,
			   [ access(execute)
			   ]),
	input_handle(Request, ScriptInput),
	findall(Name=Value,
		env(Name,
		    [ script_file_name(Script)
		    | Request
		    ], Value),
		Env),
	debug(http(cgi), 'Environment: ~w', [Env]),
	process_create(Script, Argv,
		       [ stdin(ScriptInput),
			 stdout(pipe(CGI)),
			 stderr(std),
			 env(Env),
			 process(PID)
		       ]),
	setup_input(ScriptInput, Request),
	set_stream(CGI, encoding(octet)),
	debug(http(cgi), 'Waiting for CGI data ...', []),
	maplist(header_option, Options),
	call_cleanup(copy_cgi_data(CGI, current_output, Options),
		     cgi_cleanup(Script, CGI, PID)), !.

%%	header_option(+Option) is det.
%
%	Write additional HTTP headers.

header_option(transfer_encoding(Encoding)) :- !,
	format('Transfer-encoding: ~w\r\n', [Encoding]).
header_option(_).

%%	cgi_cleanup(+Script, +ScriptStream, +PID) is det.
%
%	Cleanup the CGI process and close  the   stream  use to read the
%	output of the CGI process. Note that  we close the output first.
%	This deals with the  possibility  that   the  client  reset  the
%	connection, copy_cgi_data/3 returns and exception   and  we wait
%	for the process that never  ends.   By  closing  our stream, the
%	process will receive a sigpipe if it continues writing.

cgi_cleanup(Script, ScriptStream, PID) :-
	close(ScriptStream),
	process_wait(PID, Status),
	debug(http(cgi), '~w ended with status ~w',
	      [Script, Status]).

%%	input_handle(+Request, -Handle) is det.
%
%	Decide what to do with the input   stream of the CGI process. If
%	this is a PUT/POST request, we must   send data. Otherwise we do
%	not redirect the script's input.

input_handle(Request, pipe(_)) :-
	memberchk(method(Method), Request),
	method_has_data(Method), !.
input_handle(_, std).

method_has_data(post).
method_has_data(put).

%%	setup_input(+ScriptInput, +Request) is det.
%
%	Setup passing of the POST/PUT data to the script.

setup_input(std, _).
setup_input(pipe(Stream), Request) :-
	memberchk(input(HTTPIn), Request),
	set_stream(Stream, encoding(octet)),
	setup_input_filters(HTTPIn, In, Request, Close),
	thread_create(copy_post_data(In, Stream, Close), _,
		      [ detached(true)
		      ]).

setup_input_filters(RawIn, In, Request, (Close2,Close1)) :-
	setup_length_filter(RawIn, In2, Request, Close1),
	setup_encoding_filter(In2, In, Request, Close2).

setup_length_filter(In0, In, Request, close(In)) :-
	memberchk(content_length(Len), Request), !,
	debug(http(cgi), 'Setting input length to ~D', [Len]),
	stream_range_open(In0, In, [size(Len)]).
setup_length_filter(In, In, _, true).

setup_encoding_filter(In0, In, Request, close(In)) :-
	memberchk(content_encoding(Enc), Request),
	z_format(Enc), !,
	debug(http(cgi), 'Adding ~w input filter', [Enc]),
	zopen(In0, In, [format(Enc), close_parent(false)]).
setup_encoding_filter(In, In, _, true).

z_format(gzip).
z_format(deflate).


%%	copy_post_data(+DataIn, -ScriptIn, :Close) is det.
%
%	Copy data from the CGI script to the client.

copy_post_data(In, Script, Close) :-
	debugging(http(cgi)), !,
	setup_call_cleanup(open('post.data', write, Debug, [type(binary)]),
			   catch(debug_post_data(In, Script, Debug),
				 E,
				 print_message(error, E)),
			   close(Debug)),
	catch(Close, E, print_message(error, E)),
	close(Script, [force(true)]).
copy_post_data(In, Script, Close) :-
	catch(copy_stream_data(In, Script), _, true),
	catch(Close, E, print_message(error, E)),
	close(Script, [force(true)]).


debug_post_data(In, Script, Debug) :-
	get_code(In, Byte),
	(   Byte == -1
	->  true
	;   put_code(Script, Byte),
	    put_code(Debug, Byte),
	    debug_post_data(In, Script, Debug)
	).


%%	copy_cgi_data(+CGI, -Out, +Options) is det.

copy_cgi_data(CGI, Out, Options) :-
	debugging(http(cgi)), !,
	maplist(set_cgi_stream(Out), Options),
	setup_call_cleanup(open('cgi.out', write, Debug, [type(binary)]),
			   debug_cgi_data(CGI, Out, Debug),
			   close(Debug)).
copy_cgi_data(CGI, Out, Options) :-
	maplist(set_cgi_stream(Out), Options),
	copy_stream_data(CGI, Out).

set_cgi_stream(Out, buffer(Buffer)) :- !,
	set_stream(Out, buffer(Buffer)).
set_cgi_stream(_, _).

debug_cgi_data(CGI, Out, Debug) :-
	get_code(CGI, Byte),
	(   Byte == -1
	->  true
	;   put_code(Out, Byte),
	    put_code(Debug, Byte),
	    debug_cgi_data(CGI, Out, Debug)
	).


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
	memberchk(path_info(PathInfo0), Request),
	ensure_leading_slash(PathInfo0, PathInfo).
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

