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

:- module(plweb_page,
	  [ sidebar//0,
	    server_address//0
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(pldoc/doc_index)).
:- use_module(wiki).
:- use_module(openid).

%%	user:body(+Style, +Body)//
%
%	Redefine body behaviour

:- multifile
	user:body//2.

user:body(wiki, Body) --> !,
	user:body(wiki(default), Body).
user:body(wiki(Arg), Body) --> !,
	html(body(class(wiki),
		  [ \html_requires(plweb),
		    \shortcut_icons,
		    div(class(sidebar), \sidebar),
		    div(class(righthand),
			[ \current_user(Arg),
			  \doc_links([], [search_options(false)]),
			  div(class(content), Body),
			  div(class(footer), \server_address)
			])
		  ])),
	html_receive(script).
user:body(plain, Body) --> !,
	html(body(class(wiki), Body)).
user:body(_, Body) -->
	html(body(class(pldoc),
		  [ \html_requires(plweb),
		    \shortcut_icons,
		    div(class(sidebar), \sidebar),
		    div(class(righthand),
			[ \current_user,
			  div(class(content), Body),
			  div(class(footer), \server_address)
			])
		  ])),
	html_receive(script).

shortcut_icons -->
	{ http_absolute_location(icons('favicon.ico'), FavIcon, []),
	  http_absolute_location(root('apple-touch-icon.png'), TouchIcon, [])
	},
	html_post(head,
		  [ link([ rel('shortcut icon'), href(FavIcon) ]),
		    link([ rel('apple-touch-icon'), href(TouchIcon) ])
		  ]).

%%	sidebar//
%
%	Emit the sidebar with logo and menu

sidebar -->
	html([ div(class(logo),
		   a(href('http://www.swi-prolog.org'),
		     img([id(logo),
			  border(0),
			  alt('SWI-Prolog logo'),
			  src('/icons/swipl.png')
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

