/*  File:    page.pl
    Author:  Jan Wielemaker
    Created: Jan  9 2009
    Purpose: Main page layout
*/

:- module(plweb_page,
	  [ sidebar//0,
	    server_address//0
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(wiki).

%	user:body(Body)//
%
%	Redefine body behaviour

:- multifile
	user:body//1.

user:body(Body) -->
	html(body([ \html_requires(plweb),
		    div(class(sidebar), \sidebar),
		    div(class(content), Body),
		    div(class(footer), \server_address)
		  ])).


%%	sidebar//
%
%	Emit the sidebar with logo and menu

sidebar -->
	html([ div(class(logo),
		   a(href('http://www.swi-prolog.org'),
		     img([id(logo),
			  border(0),
			  src('/icons/swipl.jpeg')
			 ]))),
	       div(class(menu), \menu)
	     ]).

%%	menu//
%
%	Generate the sidebar menu

menu -->
	{ menu(DOM) },
	html(DOM).

menu(DOM) :-
	nb_current(pldoc_file, OrgFile),
	menu_file(OrgFile, MenuFile), !,
	wiki_file_to_dom(MenuFile, DOM).
menu(DOM) :-
	absolute_file_name(document_root('menu.txt'),
			   MenuFile,
			   [ access(read)
			   ]),
	wiki_file_to_dom(MenuFile, DOM).
menu([]).

menu_file(Base, MenuFile) :-
	parent(Base, Dir), Dir \== Base,
	concat_atom([Dir, /, 'menu.txt'], MenuFile),
	exists_file(MenuFile).

parent(Base, Base).
parent(Base, Parent) :-
	file_directory_name(Base, Dir),
	Dir \== Base,
	parent(Dir, Parent).


%%	server_address//
%
%	Emit information about the server

server_address -->
	{ prolog_version(Version)
	},
	html(['Powered by ',
	      a(href('http://www.swi-prolog.org'), 'SWI-Prolog'), ' ',
	      Version
	     ]).

prolog_version(Version) :-
	current_prolog_flag(version_git, Version), !.
prolog_version(Version) :-
	current_prolog_flag(version_data, swi(Ma,Mi,Pa,_)),
	format(atom(Version), '~w.~w.~w', [Ma,Mi,Pa]).

