/*  File:    plweb.pl
    Author:  Jan Wielemaker
    Created: Jan  8 2009
    Purpose: Create the Prolog (read-only) Wiki
*/

:- module(plweb,
	  [ server/0
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_error)).
:- use_module(library(settings)).
:- use_module(library(error)).

:- use_module(parms).

:- http_handler(root(.), serve_page, [prefix]).

		 /*******************************
		 *            SERVER		*
		 *******************************/

server :-
	setting(http:port, Port),
	server([port(Port)]).

server(Options) :-
	http_server(http_dispatch, Options).


		 /*******************************
		 *	      SERVICES		*
		 *******************************/

%%	serve_page(+Request)
%
%	HTTP handler for pages

serve_page(Request) :-
	http_absolute_location(root(.), Root, []),
	memberchk(path(Path), Request),
	atom_concat(Root, Relative, Path),
	setting(http:served_file_extensions, Exts),
	absolute_file_name(document_root(Relative),
			   File,
			   [ extensions(['',txt|Exts]),
			     access(read)
			   ]),
	absolute_file_name(document_root(.), DocRoot),
	(   atom_concat(DocRoot, _, File)
	->  file_name_extension(_, Ext, File),
	    serve_file(Ext, File, Request)
	;   permission_error(access, http_location, Path)
	).
	
%%	serve_file(+Extension, +File, +Request) is det.
%
%	Serve the requested file.

serve_file(Ext, File, Request) :-	% serve plain files
	setting(http:served_file_extensions, Exts),
	memberchk(Ext, Exts), !,
	http_reply_file(File, [], Request).
