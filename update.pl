/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

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

:- module(web_update, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/html_write)).
:- use_module(library(readutil)).
:- use_module(library(process)).

:- http_handler(root(update), update, []).

%%	update(+Request)
%
%	HTTP Handler for /update.  Performs  a   GIT  pull  and a Prolog
%	make/0.

update(Request) :-
	(   http_authenticate(basic(plweb(passwd)), Request, _User)
	->  true
	;   throw(http_reply(authorise(basic, 'Admin user')))
	),
	reply_html_page(title('Server update'),
			[ h1('Server update'),
			  hr([]),
			  h2('GIT'),
			  \git_update,
			  h2('make'),
			  \make
			]).


%%	git_update//
%
%	Run =|git update|=, collecting the output

git_update -->
	{ process_create(path(git), [pull],
			 [ stdout(pipe(Out)),
			   stderr(pipe(Error))
			 ]),
	  read_stream_to_codes(Out, OutCodes),
	  read_stream_to_codes(Error, ErrorCodes),
	  close(Out),
	  close(Error)
	},
	output('', informational, OutCodes),
	output('', error, ErrorCodes).

output(_Prefix, _Class, Codes) -->
	{ Codes == [] }, !.
output(Prefix, Class, Codes) -->
	html(pre(class(Class),
		 [ Prefix, '~s'-[Codes] ])).

%%	make//
%
%	Run make, collecting output

make -->
	{ collect_messages(make, Messages)
	},
	messages(Messages).


:- thread_local
	message/2.

collect_messages(Goal, Messages) :-
	asserta((user:message_hook(_Term, Level, Lines) :-
			assert(message(Level, Lines))), Ref),
	call_cleanup(Goal, erase(Ref)),
	findall(Level-Lines, retract(message(Level, Lines)), Messages).

messages([]) -->
	[].
messages([H|T]) -->
	message(H),
	messages(T).

message(Level-Lines) -->
	html(div(class(Level), \html_message_lines(Lines))).

html_message_lines([]) -->
	[].
html_message_lines([nl|T]) --> !,
	html([br([])]),
	html_message_lines(T).
html_message_lines([flush]) -->
	[].
html_message_lines([Fmt-Args|T]) --> !,
	{ format(string(S), Fmt, Args)
	},
	html([S]),
	html_message_lines(T).
html_message_lines([Fmt|T]) --> !,
	{ format(string(S), Fmt, [])
	},
	html([S]),
	html_message_lines(T).

