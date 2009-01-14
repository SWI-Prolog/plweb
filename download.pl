/*  File:    download.pl
    Author:  Jan Wielemaker
    Created: Jan 14 2009
    Purpose: Provide download links
*/

:- module(plweb_download, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/dcg_basics)).
:- use_module(wiki).

:- http_handler(root(download/devel), download, []).

download(Request) :-
	memberchk(path(Path), Request),
	http_absolute_location(root(download), DownLoadRoot, []),
	atom_concat(DownLoadRoot, DownLoadDir, Path),
	absolute_file_name(download(DownLoadDir),
			   Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	list_downloads(Dir).

%%	list_downloads(+Directory)

list_downloads(Dir) :-
	reply_html_page(title('SWI-Prolog downloads'),
			[ \wiki(Dir, 'header.txt'),
			  table(class(downloads),
				\download_table(Dir)),
			  \wiki(Dir, 'footer.txt')
			]).

wiki(Dir, File) -->
	{ concat_atom([Dir, /, File], WikiFile),
	  access_file(WikiFile, read), !,
	  wiki_file_to_dom(WikiFile, DOM)
	},
	html(DOM).
wiki(_, _) -->
	[].

download_table(Dir) -->
	list_files(Dir, bin, 'Binaries'),
	list_files(Dir, src, 'Sources'),
	list_files(Dir, doc, 'Documentation').

list_files(Dir, SubDir, Label) -->
	{ concat_atom([Dir, /, SubDir], Directory),
	  atom_concat(Directory, '/*', Pattern),
	  expand_file_name(Pattern, Files),
	  classsify_files(Files, Classified)
	},
	html(tr(td([class(header), colspan(3)], Label))),
	list_files(Classified).
	
list_files([]) --> [].
list_files([H|T]) -->
	list_file(H),
	list_files(T).

list_file(File) -->
	html(tr([ td(\file_icon(File)),
		  td(\file_size(File)),
		  td(\file_description(File))
		])).

file_icon(file(Type, PlatForm, _, _, _)) -->
	{ icon_for_file(Type, PlatForm, Icon, Alt),
	  http_absolute_location(icons(Icon), HREF, [])
	},
	html(img([src(HREF), alt(Alt)])).

icon_for_file(bin, linux(_),	  
	      'linux32.gif', 'Linux RPM').
icon_for_file(bin, macos(_,_),
	      'mac.gif', 'MacOSX version').
icon_for_file(bin, windows(win32),
	      'win32.gif', 'Windows version (32-bits)').
icon_for_file(bin, windows(win64),
	      'win64.gif', 'Windows version (64-bits)').
icon_for_file(src, _,
	      'src.gif', 'Source archive').
icon_for_file(_, pdf,
	      'pdf.gif', 'PDF file').


file_size(file(_, _, _, _, Path)) -->
	{ size_file(Path, Bytes)
	},
	html('~D bytes'-[Bytes]).
	  
file_description(file(bin, PlatForm, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ 'SWI-Prolog/XPCE ', \version(Version), ' for ',
		   \platform(PlatForm)
		 ]),
	       \platform_notes(PlatForm, Path)
	     ]).
file_description(file(src, _, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ 'SWI-Prolog source for ', \version(Version)
		 ])
	     ]).
file_description(file(doc, _, Version, _, Path)) -->
	{ down_file_href(Path, HREF)
	},
	html([ a(href(HREF),
		 [ 'SWI-Prolog ', \version(Version),
		   ' reference manual in PDF'
		 ])
	     ]).

version(version(Major, Minor, Patch)) -->
	html(b('~w.~w.~w'-[Major, Minor, Patch])).

down_file_href(Path, HREF) :-
	absolute_file_name(download(.),
			   Dir,
			   [ file_type(directory),
			     access(read)
			   ]),
	atom_concat(Dir, Local, Path),
	http_absolute_location(download(Local), HREF, []).
			     
platform(macos(Name, CPU)) -->
	html(['MacOSX ', \html_macos_version(Name), ' on ', CPU]).
platform(windows(win32)) -->
	html(['Windows NT/2000/XP/Vista']).
platform(windows(win64)) -->
	html(['Windows XP/Vista 64-bit edition']).

html_macos_version(tiger)   --> html('10.4 (tiger)').
html_macos_version(leopard) --> html('10.5 (leopard)').
html_macos_version(OS)	    --> html(OS).

%%	platform_notes(+Platform, +Path) is det.
%
%	Include notes on the platform. These notes  are stored in a wiki
%	file in the same directory as the download file.

platform_notes(Platform, Path) -->
	{ file_directory_name(Path, Dir),
	  platform_note_file(Platform, File),
	  concat_atom([Dir, /, File], NoteFile),
	  access_file(NoteFile, read), !,
	  wiki_file_to_dom(NoteFile, DOM)
	},
	html(DOM).
platform_notes(_, _) -->
	[].

platform_note_file(linux(_,_),	   'linux.txt').
platform_note_file(windows(win32), 'win32.txt').
platform_note_file(windows(win64), 'win64.txt').
platform_note_file(macos(_,_),	   'macosx.txt').

		 /*******************************
		 *	   CLASSIFY FILES	*
		 *******************************/

classsify_files([], []).
classsify_files([H0|T0], [H|T]) :-
	classsify_file(H0, H), !,
	classsify_files(T0, T).
classsify_files([_|T0], T) :-
	classsify_files(T0, T).

%%	classsify_file(+Path, -Term) is semidet.

classsify_file(Path, file(Type, Platform, Version, Name, Path)) :-
	file_base_name(Path, Name),
	atom_codes(Name, Codes),
	phrase(file(Type, Platform, Version), Codes).

file(bin, macos(OSVersion, CPU), Version) -->
	"swi-prolog-devel-", long_version(Version), "-",
	macos_version(OSVersion), "-", 
	macos_cpu(CPU), "-",
	"mpkg.zip", !.
file(bin, windows(WinType), Version) -->
	win_type(WinType), "pl",
	short_version(Version),
	".exe", !.
file(bin, linux(rpm, suse), Version) -->
	"pl-", long_version(Version), "-", digits(_Build),
	".i586.rpm", !.
file(src, tgz, Version) -->
	"pl-", long_version(Version), ".tar.gz", !.
file(doc, pdf, Version) -->
	"SWI-Prolog-", long_version(Version), ".pdf", !.

macos_version(tiger)   --> "tiger".
macos_version(leopard) --> "leopard".

macos_cpu(ppc)   --> "ppc".
macos_cpu(intel) --> "intel".

win_type(win32) --> "w32".
win_type(win64) --> "w64".

long_version(version(Major, Minor, Patch)) -->
	int(Major, 1), ".", int(Minor, 1), ".", int(Patch, 2), !.
	
int(Value, MaxDigits) -->
	digits(Digits),
	{ length(Digits, Len),
	  Len =< MaxDigits,
	  number_codes(Value, Digits)
	}.

short_version(version(Major, Minor, Patch)) -->
	digits(Digits),
	{   Digits = [D1,D2,D3]
	->  number_codes(Major, [D1]),
	    number_codes(Minor, [D2]),
	    number_codes(Patch, [D3])
	;   Digits = [D1,D2,D3,D4]
	->  number_codes(Major, [D1]),
	    number_codes(Minor, [D2]),
	    number_codes(Patch, [D3,D4])
	}.
			 
	  
