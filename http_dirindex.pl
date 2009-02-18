/*  File:    http_dirindex.pl
    Author:  Jan Wielemaker
    Created: Feb 17 2009
    Purpose: Create directory indices
*/

:- module(http_dirindex,
	  [ http_dirindex/2		% +Request, +PhysicalDir
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(apply)).
:- use_module(library(http/html_head)).


%%	http_dirindex(+Request, +Dir) is det.
%
%	Provide a directory listing for Request, assuming it is an index
%	for the physical directrory Dir. If   the  request-path does not
%	end with /, first return a moved reply.

http_dirindex(Request, Dir) :-
	memberchk(path(Path), Request),
	(   atom_concat(PlainPath, /, Path)
	->  dir_index(Dir, [title(['Index of ', PlainPath])])
	;   atom_concat(Path, /, NewLocation),
	    throw(http_reply(moved(NewLocation)))
	).

dir_index(Dir, Options) :-
	directory_members(Dir, SubDirs, Files),
	option(title(Title), Options, Dir),
	reply_html_page(title(Title),
			[ \html_requires(http_dirindex),
			  h1(Title),
			  table(class(dirindex),
				[ \dirindex_title,
				  \back
				| \dirmembers(SubDirs, Files)
				])
			]).

directory_members(Dir, Dirs, Files) :-
	atom_concat(Dir, '/*', Pattern),
	expand_file_name(Pattern, Matches),
	partition(exists_directory, Matches, Dirs, Files).

dirindex_title -->
	html(tr(class(dirindex_header),
		[ th(class(icon),     ''),
		  th(class(name),     'Name'),
		  th(class(modified), 'Last modified'),
		  th(class(size),     'Size')
		])).

back -->
	html(tr([ \icon_cell('back.png', '[UP]'),
		  \name_cell(.., 'Up'),
		  td(class(modified), -),
		  td(class(size),     -)
		])).

dirmembers(Dirs, Files) -->
	dir_rows(Dirs, odd, End),
	file_rows(Files, End, _).

dir_rows([], OE, OE) --> [].
dir_rows([H|T], OE0, OE) -->
	dir_row(H, OE0),
	{ oe(OE0, OE1) }, 
	dir_rows(T, OE1, OE).

file_rows([], OE, OE) --> [].
file_rows([H|T], OE0, OE) -->
	file_row(H, OE0),
	{oe(OE0, OE1)},
	file_rows(T, OE1, OE).

oe(odd, even).
oe(even, odd).

dir_row(Dir, OE) -->
	{ file_base_name(Dir, Name)
	},
	html(tr(class(OE),
		[ \icon_cell('folder.png', '[DIR]'),
		  \name_cell(Name, Name),
		  \modified_cell(Dir),
		  td(class(size), -)
		])).


file_row(File, OE) -->
	{ file_base_name(File, Name),
	  file_name_extension(_, Ext, Name),
	  file_type_icon(Ext, IconName)
	},
	html(tr(class(OE),
		[ \icon_cell(IconName, '[FILE]'),
		  \name_cell(Name, Name),
		  \modified_cell(File),
		  td(class(size), \size(File))
		])).

icon_cell(IconName, Alt) -->
	{ http_absolute_location(icons(IconName), Icon, [])
	},
	html(td(class(icon), img([src(Icon), alt(Alt)]))).


name_cell(Ref, Name) -->
	html(td(class(name), a(href(Ref), Name))).


modified_cell(Name) -->
	{ time_file(Name, Stamp),
	  format_time(string(Date), '%+', Stamp)
	},
	html(td(class(modified), Date)).

size(Name) -->
	{ size_file(Name, Size)
	},
	html('~D'-[Size]).

file_type_icon(Ext, Icon) :-
	ext_icon(Ext, Icon), !.
file_type_icon(_, 'generic.png').

ext_icon(pdf, 'layout.png').
ext_icon(c, 'c.png').
ext_icon(gz, 'compressed.png').
ext_icon(tgz, 'compressed.png').
ext_icon(zip, 'compressed.png').

		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

:- html_resource(http_dirindex,
		 [ virtual(true),
		   requires([ css('dirindex.css')
			    ])
		 ]).
