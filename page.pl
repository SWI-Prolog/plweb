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
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_html), except([file//1])).

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
	b_getval(pldoc_file, OrgFile),
	menu_file(OrgFile, MenuFile), !,
	read_file_to_codes(MenuFile, String, []),
	b_setval(pldoc_file, MenuFile),
	call_cleanup(wiki_string_to_dom(String, [], DOM),
		     b_setval(pldoc_file, OrgFile)).
menu([]).

menu_file(Base, MenuFile) :-
	parent(Base, Dir), Dir \== Base,
	concat_atom([Dir, /, 'menu.txt'], MenuFile),
	exists_file(MenuFile).

parent(Base, Base).
parent(Base, Parent) :-
	file_directory_name(Base, Dir),
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


		 /*******************************
		 *	     RENDERING		*
		 *******************************/

%%	file(+Path)//
%
%	Trap translation of \file(+Path)

file(Path) -->
	{ file_base_name(Path, File),
	  file_name_extension(Label, txt, File)
	}, !,
	file(File, [label(Label)]).
file(File) -->
	file(File, []).
