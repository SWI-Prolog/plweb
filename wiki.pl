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
	    wiki_page_title/2,		% +Location, -Title
	    index_wiki_pages/0,		%
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
:- use_module(library(apply)).
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
	prolog:doc_object_page//2,
	prolog:doc_category/3,
	prolog:doc_file_index_header//2.

prolog:doc_object_summary(wiki(Location), wiki, wiki, Summary) :-
	wiki_page_title(Location, Summary).

:- dynamic
	wiki_page_title_cache/2,
	wiki_pages_indexed/1.

%%	wiki_page_title(?Location, ?Title) is nondet.
%
%	True when Title is the title of the wiki page at Location.

wiki_page_title(Location, Title) :-
	wiki_pages_indexed(_), !,
	wiki_page_title_cache(Location, Title).
wiki_page_title(Location, Title) :-
	nonvar(Location), !,
	(   wiki_page_title_cache(Location, TitleRaw)
	->  Title = TitleRaw
	;   location_wiki_file(Location, File),
	    catch(wiki_file_to_dom(File, DOM), E,
		  ( print_message(warning, E),
		    fail
		  )),
	    dom_title(DOM, TitleRaw)
	->  assertz(wiki_page_title_cache(Location, TitleRaw)),
	    Title = TitleRaw
	;   format(atom(TitleRaw), 'Wiki page at "~w"', Location)
	->  assertz(wiki_page_title_cache(Location, TitleRaw)),
	    Title = TitleRaw
	).
wiki_page_title(Location, Title) :-
	index_wiki_pages,
	wiki_page_title(Location, Title).


%%	dom_title(+DOM, -Title) is semidet.
%
%	Get the title as an atom from a parsed wiki page.
%
%	@tbd	Currently assumes no markup in the title.

dom_title([h1(_, TitleList)|_], Title) :-
	maplist(to_atom, TitleList, TitleList2),
	atomic_list_concat(TitleList2, Title).

to_atom(Atomic, Atomic) :- atomic(Atomic).
to_atom(predref(Name/Arity), Label) :-
	atomic_list_concat([Name,/,Arity], Label).

prolog:doc_object_link(wiki(Location), _Options) -->
	{ wiki_page_title(Location, Title) },
	html([ '[wiki] ', Title ]).

prolog:doc_object_page(wiki(Location), _Options) -->
	{ http_current_request(Request),
	  http_redirect(see_other, root(Location), Request)
	}.

prolog:doc_category(wiki, 60, 'Wiki pages from the website').

prolog:doc_file_index_header(wiki, _) --> [].

%%	index_wiki_pages
%
%	Create a (title) index of  the   available  wiki  pages. This is
%	started from server/1 in a background thread.

index_wiki_pages :-
	wiki_pages_indexed(_), !.
index_wiki_pages :-
	with_mutex(index_wiki_pages,
		   index_wiki_pages_sync).

index_wiki_pages_sync :-
	wiki_pages_indexed(_).
index_wiki_pages_sync :-
	wiki_locations(Locations),
	maplist(wiki_page_title, Locations, _Titles),
	get_time(Now),
	asserta(wiki_pages_indexed(Now)).


%%	wiki_locations(-Locations) is det.
%
%	True when Files is a list of all .txt files on the site.

wiki_locations(Files) :-
	findall(Dir, absolute_file_name(
			 document_root(.), Dir,
			 [access(read), file_type(directory)]),
		RootDirs),
	maplist(wiki_locations, RootDirs, NestedFiles),
	append(NestedFiles, Files).

wiki_locations(Dir, Files) :-
	phrase(wiki_locations(Dir, Dir), Files).

wiki_locations([], _) --> !.
wiki_locations([H|T], Root) --> !,
	wiki_locations(H, Root),
	wiki_locations(T, Root).
wiki_locations(CurrentDir, Root) -->
	{ exists_directory(CurrentDir), !,
	  directory_files(CurrentDir, Members),
	  exclude(special, Members, Members2),
	  maplist(directory_file_path(CurrentDir), Members2, MemberPaths)
	},
	wiki_locations(MemberPaths, Root).
wiki_locations(Entry, Root) -->
	{ file_name_extension(_, Ext, Entry),
	  wiki_extension(Ext), !,
	  directory_file_path(Root, Wiki, Entry)
	},
	[Wiki].
wiki_locations(_, _) --> [].

wiki_extension(txt).

special(.).
special(..).
