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
:- use_module(library(debug)).
:- use_module(library(apply)).

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
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read)
			   ]),
	absolute_file_name(document_root(.), DocRoot),
	(   atom_concat(DocRoot, _, File)
	->  file_name_extension(_, Ext, File),
	    debug(plweb, 'Serving ~q; ext=~q', [File, Ext]),
	    serve_file(Ext, File, Request)
	;   permission_error(access, http_location, Path)
	).
	
%%	serve_file(+Extension, +File, +Request) is det.
%
%	Serve the requested file.

serve_file('',  DirSpec, Request) :-
	(   atom_concat(Dir, /, DirSpec)
	->  serve_directory(Dir, Request)
	;   exists_directory(DirSpec),
	    memberchk(path(Path), Request),
	    atom_concat(Path, /, NewLocation),
	    throw(http_reply(moved(NewLocation)))
	).
serve_file(Ext, File, Request) :-	% serve plain files
	setting(http:served_file_extensions, Exts),
	memberchk(Ext, Exts), !,
	http_reply_file(File, [], Request).

%%	serve_directory(+Dir, +Request) is det.
%
%	Serve a directory

serve_directory(Dir, Request) :-
	concat_atom([Dir, /, 'index.html'], Index),
	exists_file(Index), !,
	http_reply_file(Index, [], Request).
serve_directory(Dir, Request) :-
	setting(http:served_file_extensions, Exts),
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Matches),
	select_directories(Matches, Dirs, Files),
	include(has_extension_in(Exts), Files, OkFiles),
	append(Dirs, OkFiles, Entries),
	memberchk(path(Path), Request),
	atom_concat(Dir, /, DirSlash),
	reply_html_page(title(['Listing for ', Path]),
			table([ \up,
			        \list_entries(Entries, DirSlash)
			      ])).

has_extension_in(Exts, File) :-
	file_name_extension(_, Ext, File),
	memberchk(Ext, Exts).

%%	select_directories(+Matches, -Dirs, -Files) is det.
%
%	Partition the Matches into directories, represented as dir(Path)
%	and files.

select_directories([], [], []).
select_directories([H|T], Dirs, Files) :-
	(   exists_directory(H)
	->  Dirs = [dir(H)|D1],
	    select_directories(T, D1, Files)
	;   Files = [H|F1],
	    select_directories(T, Dirs, F1)
	).


up -->
	html(tr([ td(a(href(..), ..))
		])).

list_entries([], _) --> [].
list_entries([H|T], Dir) -->
	list_entry(H, Dir),
	list_entries(T, Dir).

list_entry(dir(Dir), Parent) -->
	{ atom_concat(Parent, Local, Dir)
	},
	html(tr([ td(a(href(Local+'/'), [Local, /]))
		])).
list_entry(File, Dir) -->
	{ atom_concat(Dir, Local, File)
	},
	html(tr([ td(a(href(Local), Local))
		])).
