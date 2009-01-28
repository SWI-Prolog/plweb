/*  File:    plweb.pl
    Author:  Jan Wielemaker
    Created: Jan  8 2009
    Purpose: Create the Prolog (read-only) Wiki
*/

:- module(plweb,
	  [ server/0
	  ]).
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_man)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_error)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(occurs)).

:- use_module(parms).
:- use_module(page).
:- use_module(download).
:- use_module(wiki).
:- use_module(http_cgi).

:- http_handler(root(.),   serve_page,     [prefix, priority(10)]).
:- http_handler(root(man), manual_file,    [prefix, priority(10)]).

/** <module> Server for PlDoc wiki pages and SWI-Prolog website

@tbd	Turn directory listing into a library.
*/

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
%	HTTP handler for files below document-root.

serve_page(Request) :-
	http_location_by_id(serve_page, Root),
	memberchk(path(Path), Request),
	atom_concat(Root, Relative, Path),
	find_file(Relative, File), !,
	absolute_file_name(document_root(.), DocRoot),
	(   atom_concat(DocRoot, _, File)
	->  serve_file(File, Request)
	;   permission_error(access, http_location, Path)
	).
serve_page(Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).
	
%%	find_file(+Relative, -File) is det.
%
%	Translate Relative into a File in the document-root tree. If the
%	given extension is .html, also look for   .txt files that can be
%	translated into HTML.

find_file(Relative, File) :-
	file_name_extension(Base, html, Relative),
	file_name_extension(Base, txt, WikiFile),
	absolute_file_name(document_root(WikiFile),
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.
find_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]).


%%	serve_file(+File, +Request) is det.
%%	serve_file(+Extension, +File, +Request) is det.
%
%	Serve the requested file.

serve_file(File, Request) :-
	file_name_extension(_, Ext, File),
	debug(plweb, 'Serving ~q; ext=~q', [File, Ext]),
	serve_file(Ext, File, Request).

serve_file('',  DirSpec, Request) :-
	(   atom_concat(Dir, /, DirSpec)
	->  serve_directory(Dir, Request)
	;   exists_directory(DirSpec),
	    memberchk(path(Path), Request),
	    atom_concat(Path, /, NewLocation),
	    throw(http_reply(moved(NewLocation)))
	).
serve_file(txt, File, _Request) :- !,
	read_file_to_codes(File, String, []),
	b_setval(pldoc_file, File),
	call_cleanup(serve_wike(String),
		     nb_delete(pldoc_file)).
serve_file(Ext, File, Request) :-	% serve plain files
	setting(http:served_file_extensions, Exts),
	memberchk(Ext, Exts), !,
	http_reply_file(File, [], Request).

%%	serve_wiki(+String) is det.
%
%	Emit page from wiki content in String.

serve_wike(String) :-
	wiki_string_to_dom(String, [], DOM),
	(   sub_term(h1(_, Title), DOM)
	->  true
	;   Title = 'SWI-Prolog'
	),
	reply_html_page([ title(Title)
			],
			DOM).

%%	serve_directory(+Dir, +Request) is det.
%
%	Serve a directory

serve_directory(Dir, Request) :-
	setting(http:index_files, Indices),
	member(Index, Indices),
	concat_atom([Dir, /, Index], File),
	access_file(File, read), !,
	serve_file(File, Request).
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

%%	manual_file(+Request) is det.
%
%	HTTP handler for /man/file.html

manual_file(Request) :-
	memberchk(path(Path), Request),
	http_location_by_id(manual_file, Root),
	atom_concat(Root, Relative, Path),
	atom_concat('doc/Manual', Relative, Man),
	absolute_file_name(swi(Man),
			   ManFile,
			   [ access(read),
			     file_errors(fail)
			   ]), !,
	reply_html_page(title('SWI-Prolog manual'),
			\man_page(section(_,_,ManFile), [])).
manual_file(Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).
