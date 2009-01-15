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
	(   nb_current(pldoc_file, OrgFile)
	->  b_setval(pldoc_file, File),
	    call_cleanup(wiki_string_to_dom(String, [], DOM),
			 b_setval(pldoc_file, OrgFile))
	;   b_setval(pldoc_file, File),
	    call_cleanup(wiki_string_to_dom(String, [], DOM),
			 nb_delete(pldoc_file))
	).


		 /*******************************
		 *	     RENDERING		*
		 *******************************/

%%	file(+Path, Options)//
%
%	Trap translation of \file(+Path, Options)

file(Path, Options) -->
	{ \+ option(label(_), Options),
	  file_base_name(Path, File),
	  file_name_extension(Label, txt, File), !,
	  file_href(Options, Options1)
	},
	pldoc_html:file(Path, [label(Label)|Options1]).
file(File, Options) -->
	{ file_href(Options, Options1)
	},
	pldoc_html:file(File, Options1).


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
	    
	
