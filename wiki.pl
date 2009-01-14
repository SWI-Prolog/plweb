/*  File:    wiki.pl
    Author:  Jan Wielemaker
    Created: Jan 14 2009
    Purpose: WIKI Stuff
*/

:- module(plweb_wiki,
	  [ wiki_file_to_dom/2,		% +File, -DOM
	    file//2			% +File, +Options
	  ]).
:- reexport(library(pldoc/doc_html),
	    except([file//2])).

:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(readutil)).


%%	wiki_file_to_dom(+File, +DOM) is det.
%
%	DOM is the HTML dom representation for the content of File.

wiki_file_to_dom(File, DOM) :-
	read_file_to_codes(File, String, []),
	b_getval(pldoc_file, OrgFile),
	b_setval(pldoc_file, File),
	call_cleanup(wiki_string_to_dom(String, [], DOM),
		     b_setval(pldoc_file, OrgFile)).


		 /*******************************
		 *	     RENDERING		*
		 *******************************/

%%	file(+Path, Options)//
%
%	Trap translation of \file(+Path, Options)

file(Path, Options) -->
	{ \+ option(label(_), Options),
	  file_base_name(Path, File),
	  file_name_extension(Label, txt, File)
	}, !,
	pldoc_html:file(Path, [label(Label)|Options]).
file(File, Options) -->
	pldoc_html:file(File, Options).

