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

:- module(plweb_wiki,
	  [ wiki_file_to_dom/2,		% +File, -DOM
	    wiki_file_codes_to_dom/3,	% +Codes, +File, -DOM
	    file//2,			% +File, +Options
	    include//3			% +Object, +Type, +Options
	  ]).
:- reexport(library(pldoc/doc_html),
	    except([ file//2,
		     include//3
		   ])).

:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(readutil)).
:- use_module(library(option)).
:- use_module(wiki_edit).

:- predicate_options(file//2, 2,
		     [ absolute_path(atom),
		       label(any)
		     ]).
:- predicate_options(include//3, 3,
		     [pass_to(pldoc_html:include/5, 3)]).

%%	wiki_file_to_dom(+File, +DOM) is det.
%
%	DOM is the HTML dom representation for the content of File.

wiki_file_to_dom(File, DOM) :-
	read_file_to_codes(File, String, []),
	wiki_file_codes_to_dom(String, File, DOM).

%%	wiki_codes_to_dom(+Codes, +File, -DOM)
%
%	DOM is the HTML dom representation for Codes that originate from
%	File.

wiki_file_codes_to_dom(String, File, DOM) :-
	(   nb_current(pldoc_file, OrgFile)
	->  setup_call_cleanup(
		b_setval(pldoc_file, File),
		wiki_codes_to_dom(String, [], DOM),
		b_setval(pldoc_file, OrgFile))
	;   setup_call_cleanup(
		b_setval(pldoc_file, File),
		wiki_codes_to_dom(String, [], DOM),
		nb_delete(pldoc_file))
	).


		 /*******************************
		 *	     RENDERING		*
		 *******************************/

%%	include(+Object, +Type, +Options)//

include(Object, Type, Options) -->
	pldoc_html:include(Object, Type,
			   [ map_extension([txt-html])
			   | Options
			   ]).

%%	file(+Path, Options)//
%
%	Trap translation of \file(+Path, Options)

file(Path, Options) -->
	{ \+ option(label(_), Options),
	  file_base_name(Path, File),
	  file_name_extension(Label, txt, File), !,
	  file_href(Options, Options1)
	},
	pldoc_html:file(Path,
			[ label(Label),
			  map_extension([txt-html]),
			  edit_handler(wiki_edit)
			| Options1
			]).
file(File, Options) -->
	{ file_href(Options, Options1)
	},
	pldoc_html:file(File,
			[ map_extension([txt-html]),
			  edit_handler(wiki_edit)
			| Options1
			]).


file_href(Options0, Options) :-
	\+ ( nb_current(pldoc_file, CFile),
	     CFile \== []
	   ),
	option(absolute_path(Path), Options0),
	absolute_file_name(document_root(.),
			   DocRoot,
			   [ file_type(directory),
			     access(read)
			   ]),
	atom_concat(DocRoot, DocLocal, Path), !,
	ensure_leading_slash(DocLocal, HREF),
	Options = [ href(HREF) | Options0 ].
file_href(Options, Options).

ensure_leading_slash(Path, SlashPath) :-
	(   sub_atom(Path, 0, _, _, /)
	->  SlashPath = Path
	;   atom_concat(/, Path, SlashPath)
	).

		 /*******************************
		 *     OBJECT INTEGRATION	*
		 *******************************/

:- multifile
	prolog:doc_object_summary/4,
	prolog:doc_object_link//2,
	prolog:doc_object_page//2.

prolog:doc_object_summary(wiki(Location), wiki, Location, Summary) :-
	wiki_page_title(Location, Summary).

:- dynamic
	wiki_page_title_cache/2.

wiki_page_title(Location, Title) :-
	nonvar(Location),
	(   wiki_page_title_cache(Location, TitleRaw)
	->  Title = TitleRaw
	;   location_wiki_file(Location, File),
	    wiki_file_to_dom(File, DOM),
	    dom_title(DOM, TitleRaw)
	->  assertz(wiki_page_title_cache(Location, TitleRaw)),
	    Title = TitleRaw
	).

%%	dom_title(+DOM, -Title) is semidet.
%
%	Get the title as an atom from a parsed wiki page.
%
%	@tbd	Currently assumes no markup in the title.

dom_title([h1(_, TitleList)|_], Title) :-
	atomic_list_concat(TitleList, Title).

prolog:doc_object_link(wiki(Location), _Options) -->
	{ wiki_page_title(Location, Title) },
	html([ '[wiki] ', Title ]).

prolog:prolog:doc_object_page(wiki(Location), _Options) -->
	{ http_current_request(Request),
	  http_redirect(see_other, Location, Request)
	}.
