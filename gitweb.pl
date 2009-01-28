/*  File:    git.pl
    Author:  Jan Wielemaker
    Created: Jan 28 2009
    Purpose: Server /git/ for browsing the repository
*/

:- module(gitweb, []).
:- use_module(library(http/http_dispatch)).
:- use_module(http_cgi).

:- http_handler(root(git), gitweb, [ prefix ]).
:- http_handler(root('git/gitweb.css'),
		http_reply_file(gitweb('gitweb.css'), []), []).
:- http_handler(root('git/git-logo.png'),
		http_reply_file(gitweb('git-logo.png'), []), []).
	  
%%	gitweb(+Request)
%
%	Call gitweb script

gitweb(Request) :-
	absolute_file_name(gitweb('gitweb.cgi'), ScriptPath,
			   [ access(execute)
			   ]),
	http_run_cgi(ScriptPath, Request).


:- multifile
	http_cgi:environment/2.

environment('GITWEB_CONFIG', Config) :-
	absolute_file_name(gitweb('gitweb.conf'), Config,
			   [ access(read)
			   ]).
