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

:- module(gitweb, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(apply)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(http_cgi).

/** <module> Provide gitweb support

@tbd	Also serve the GIT repository over this gateway
@tbd	Better way to locate the GIT project root
*/

:- http_handler(root('git'), gitroot, []).
:- http_handler(root('git/'), gitweb, [ prefix, spawn(cgi) ]).
:- http_handler(root('home/pl/git/'), git_http, [prefix, spawn(download)]).

%%	gitroot(+Request) is det.
%
%	Some toplevel requests are send to   /git,  while working inside
%	the repository asks for /git/. This  is   a  hack to work around
%	these problems.

gitroot(Request) :-
	http_location_by_id(gitroot, Me),
	atom_concat(Me, /, NewPath),
	include(local, Request, Parts),
	http_location([path(NewPath)|Parts], Moved),
	throw(http_reply(moved(Moved))).

local(search(_)).
local(fragment(_)).

%%	gitweb(+Request)
%
%	Call gitweb script

gitweb(Request) :-
	memberchk(path(Path), Request),
	file_base_name(Path, Base),
	resource_file(Base, File), !,
	debug(gitweb, 'Sending resource ~q', [File]),
	http_reply_file(File, [], Request).
gitweb(Request) :-
	absolute_file_name(gitweb('gitweb.cgi'), ScriptPath,
			   [ access(execute)
			   ]),
	http_run_cgi(ScriptPath, [], Request).


resource_file('gitweb.css',	 gitweb('static/gitweb.css')).
resource_file('gitweb.js',	 gitweb('static/gitweb.js')).
resource_file('git-logo.png',	 gitweb('static/git-logo.png')).
resource_file('git-favicon.png', gitweb('static/git-favicon.png')).


:- multifile
	http_cgi:environment/2.

http_cgi:environment('PROJECT_ROOT', Root) :-		% gitweb
	git_project_root(Root).
http_cgi:environment('GIT_PROJECT_ROOT', Root) :-	% git-http
	git_project_root(Root).
http_cgi:environment('GITWEB_CONFIG', Config) :-
	absolute_file_name(gitweb('gitweb.conf'), Config,
			   [ access(read)
			   ]).
http_cgi:environment('PATH', '/bin:/usr/bin:/usr/local/bin').


git_project_root(Root) :-
	absolute_file_name(plgit(.), RootDir,
			   [ access(read),
			     file_type(directory)
			   ]),
	atom_concat(RootDir, /, Root),
	debug(gitweb, 'PROJECT_ROOT = ~q', [Root]).


%%	git_http(+Request) is det.
%
%	Server files from the git tree to make this work:
%
%	    ==
%	    git clone http://www.swi-prolog.org/nl/home/pl/git/pl.git
%	    ==
%
%	Note that we must generate a   proper 404 for non-existent files
%	to make the GIT client happy.

git_http(Request) :-
	http_run_cgi(path(git),
		     [ argv(['http-backend']),
		       transfer_encoding(chunked),
		       buffer(line)
		     ],
		     Request).
