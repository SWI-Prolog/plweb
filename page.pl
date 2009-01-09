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

%%	sidebar//
%
%	Emit the sidebar with logo and menu

sidebar -->
	html([ div(class(logo), a(href('http://www.swi-prolog.org'),
				  img([id(logo),
				       border(0),
				       src('/icons/swipl.jpeg')
				      ]))),
	       div(class(menu), menu)
	     ]).

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
