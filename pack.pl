/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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

:- module(pack, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(persistency)).
:- use_module(library(aggregate)).
:- use_module(library(memfile)).

:- http_handler(root(pack/query), pack_query, []).
:- http_handler(root(pack/list),  pack_list, []).

%%	pack_query(+Request)
%
%	Handle package query requests from remote installers.  Content
%	is of type text/x-prolog.   Reply is also a Prolog term.

pack_query(Request) :-
	memberchk(content_type('text/x-prolog'), Request), !,
	peer(Request, Peer),
	setup_call_cleanup(
	    new_memory_file(MemFile),
	    ( setup_call_cleanup(
		  open_memory_file(MemFile, write, Stream),
		  http_read_data(Request, _, [to(stream(Stream))]),
		  close(Stream)),
	      setup_call_cleanup(
		  open_memory_file(MemFile, read, In),
		  read(In, Query),
		  close(In))
	    ),
	    free_memory_file(MemFile)),
	(   catch(pack_query(Query, Peer, Reply), E, true)
	->  format('Content-type: text/x-prolog~n~n'),
	    (   var(E)
	    ->	format('~q.~n', [true(Reply)])
	    ;	format('~w.~n', [exception(E)])
	    )
	;   format('Content-type: text/x-prolog~n~n'),
	    format('false.~n')
	).

peer(Request, Peer) :-
	memberchk(x_forwarded_for(Peer), Request), !.
peer(Request, PeerAtom) :-
	memberchk(peer(Peer), Request),
	peer_to_atom(Peer, PeerAtom).

peer_to_atom(ip(A,B,C,D), Atom) :-
	atomic_list_concat([A,B,C,D], '.', Atom).

pack_query(install(URL, SHA1, Info), Peer, Reply) :-
	with_mutex(pack, save_request(URL, SHA1, Info, Peer)),
	findall(ReplyInfo, install_info(URL, SHA1, ReplyInfo, []), Reply).
pack_query(locate(Pack), _, Reply) :-
	urls_for_pack(Pack, Reply).
pack_query(search(Word), _, Reply) :-
	search_packs(Word, Reply).


		 /*******************************
		 *	COMPUTATIONAL LOGIC	*
		 *******************************/

%%	install_info(+URL, +SHA1, -Info, +Seen) is nondet.
%
%	Info is relevant information  for  the   client  who  whishes to
%	install URL, which has the given   SHA1 hash. Currently provided
%	info is:
%
%	  - alt_hash(Downloads, URLs, Hash)
%	    Another file with the same (base) name was registered that
%	    has a different hash.  This file was downloaded Downloads
%	    times, resides on the given URLs (a list) and has the given
%	    Hash.
%	  - downloads(Downloads)
%	    This hash was downloaded Downloads times from a unique IP
%	    address
%	  - dependency(Token, Pack, Version, URLs, SubSeps)
%	    The requirement Token can be provided by Pack@Version, which
%	    may be downloaded from the given URLs (a list).  Pack has
%	    install info as specified by SubSeps (recursive
%	    dependencies)

install_info(_, SHA1, _, Seen) :-
	memberchk(SHA1, Seen), !, fail.
install_info(URL, SHA1, alt_hash(Downloads, URLs, Hash), _) :-
	file_base_name(URL, File),
	sha1_file(Hash, File),
	Hash \== SHA1,
	sha1_downloads(Hash, Downloads),
	sha1_urls(Hash, URLs).
install_info(_, SHA1, downloads(Count), _) :-
	sha1_downloads(SHA1, Count).
install_info(_, SHA1, dependency(Token, Pack, Version, URLs, SubDeps), Seen) :-
	sha1_requires(SHA1, Token),
	(   (   sha1_pack(Hash, Token),
		Pack = Token
	    ;	sha1_provides(Hash, Token),
		sha1_pack(Hash, Pack),
		Pack \== Token
	    ),
	    sha1_info(Hash, Info),
	    memberchk(version(Version), Info),
	    findall(URL, sha1_url(Hash, URL), URLs)
	*-> findall(SubDep, install_info(-, Hash, SubDep, [SHA1|Seen]), SubDeps)
	;   Pack = (-), Version = (-), URLs = []
	).

sha1_downloads(Hash, Count) :-
	aggregate_all(count, sha1_download(Hash, _), Count).

sha1_urls(Hash, URLs) :-
	findall(URL, sha1_url(Hash, URL), URLs).

sha1_version(Hash, Version) :-
	sha1_info(Hash, Info),
	memberchk(version(Atom), Info),
	prolog_pack:atom_version(Atom, Version).

sha1_title(Hash, Title) :-
	sha1_info(Hash, Info),
	(   memberchk(title(Title), Info)
	->  true
	;   Title = '<no title>'
	).

%%	urls_for_pack(+Pack, -Locations) is det.
%
%	@param	Locations is a list Version-URLs, sorted latest version
%		first.
%	@tbd	Handle versions with multiple hashes!

urls_for_pack(Pack, Locations) :-
	setof(SHA1, sha1_pack(SHA1, Pack), Hashes),
	map_list_to_pairs(sha1_version, Hashes, Versions),
	keysort(Versions, Sorted),
	reverse(Sorted, LastFirst),
	maplist(pack_download_url, LastFirst, Locations).

pack_download_url(Version-Hash, VersionA-URLs) :-
	prolog_pack:atom_version(VersionA, Version),
	findall(URL, sha1_url(Hash, URL), URLs).

%%	search_packs(+Search, -Packs) is det.
%
%	Search packs by keyword, returning a list
%
%		pack(Pack, Status, Version, Title, URLs).

search_packs(Search, Packs) :-
	setof(Pack, matching_pack(Search, Pack), Names), !,
	maplist(pack_search_result, Names, Packs).

matching_pack(Search, Pack) :-
	sha1_pack(SHA1, Pack),
	(   '$apropos_match'(Search, Pack)
	->  true
	;   sha1_title(SHA1, Title),
	    '$apropos_match'(Search, Title)
	).

pack_search_result(Pack, pack(Pack, p, Title, VersionA, URLs)) :-
	sha1_title(SHA1, Title),
	pack_latest_version(Pack, SHA1, Version, _Older),
	prolog_pack:atom_version(VersionA, Version),
	findall(URL, sha1_url(SHA1, URL), URLs).


		 /*******************************
		 *	     DATABASE		*
		 *******************************/

:- persistent
	sha1_pack(sha1:atom, pack:atom),
	sha1_file(sha1:atom, file:atom),
	sha1_requires(sha1:atom, token:atom),
	sha1_provides(sha1:atom, token:atom),
	sha1_info(sha1:atom, info:list),
	sha1_url(sha1:atom, url:atom),
	sha1_download(sha1:atom, peer:atom).

:- initialization
	db_attach('packs.db', []).

%%	save_request(+URL, +SHA1, +Info, +Peer)
%
%	Update the database with the given   information. We only update
%	if the request is new, which means   the  same SHA1 has not been
%	downloaded from the same Peer.

save_request(URL, SHA1, _Info, Peer) :-
	sha1_download(SHA1, Peer), !,		% already downloaded from here
	register_url(SHA1, URL).		% but maybe from a different URL
save_request(URL, SHA1, Info, Peer) :-
	memberchk(name(Pack), Info),
	register_pack(SHA1, Pack),
	register_info(SHA1, Info),
	register_url(SHA1, URL),
	assert_sha1_download(SHA1, Peer).

register_pack(SHA1, Pack) :-
	(   sha1_pack(SHA1, Pack)
	->  true
	;   assert_sha1_pack(SHA1, Pack)
	).

register_info(SHA1, Info0) :-
	sort(Info0, Info),
	(   sha1_info(SHA1, Info)
	->  true
	;   assert_sha1_info(SHA1, Info),
	    forall(member(requires(Token), Info),
		   register_requires(SHA1, Token)),
	    forall(member(provides(Token), Info),
		   register_provides(SHA1, Token))
	).

register_requires(SHA1, Token) :-
	(   sha1_requires(SHA1, Token)
	->  true
	;   assert_sha1_requires(SHA1, Token)
	).

register_provides(SHA1, Token) :-
	(   sha1_provides(SHA1, Token)
	->  true
	;   assert_sha1_provides(SHA1, Token)
	).

register_url(SHA1, URL) :-
	(   sha1_url(SHA1, URL)
	->  true
	;   assert_sha1_url(SHA1, URL),
	    file_base_name(URL, File),
	    register_file(SHA1, File)
	).

register_file(SHA1, File) :-
	(   sha1_file(SHA1, File)
	->  true
	;   assert_sha1_file(SHA1, File)
	).


		 /*******************************
		 *	     USER API		*
		 *******************************/

%%	pack_list(+Request)
%
%	List available packages.

pack_list(Request) :-
	http_parameters(Request,
			[ p(Pack, [optional(true)])
			]),
	reply_html_page(title('SWI-Prolog packages'),
			[ \pack_listing(Pack)
			]).


pack_listing(All) -->
	{ var(All), !,
	  findall(Pack, sha1_pack(_,Pack), Packs),
	  sort(Packs, Sorted)
	},
	html([ h1(class(wiki), 'Available packages'),
	       p([ 'Below is a list of known packages.  Please be aware that ',
		   'packages are ', b('not moderated'), '. Installing a pack ',
		   ' does not execute code in the pack, but simply loading a ',
		   'library from the pack may execute arbitrary code. ',
		   'More information about packages is available ',
		   a(href('http://www.swi-prolog.org/howto/Pack.html'), here),
		   '.'
		 ]),
	       \html_requires(css('pack.css')),
	       table(class(packlist),
		     [ tr([ th(id(pack),      'Pack'),
			    th(id(version),   'Version (#older)'),
			    th(id(downloads), 'Downloads (#latest)'),
			    th(id(title),     'Title')
			  ])
		     | \pack_rows(Sorted)
		     ])
	     ]).
pack_listing(Pack) -->
	html([ h1(class(wiki), 'Package "~w"'-[Pack]),
	       \html_requires(css('pack.css')),
	       \pack_info(Pack)
	     ]).


pack_rows([]) --> [].
pack_rows([H|T]) --> pack_row(H), pack_rows(T).

pack_row(Pack) -->
	{ http_link_to_id(pack_list, [p(Pack)], HREF) },
	html(tr([ td(a(href(HREF),Pack)),
		  td(\pack_version(Pack, SHA1)),
		  td(\pack_downloads(Pack, SHA1)),
		  td(\pack_title(SHA1))
		])).

pack_version(Pack, SHA1) -->
	{ pack_latest_version(Pack, SHA1, Version, Older),
	  prolog_pack:atom_version(Atom, Version)
	},
	html('~w (~d)'-[Atom,Older]).

%%	pack_latest_version(+Pack, -SHA1, -Version, -OlderCount)
%
%	True when SHA1 is the  latest  version   of  Pack  at  the given
%	Version and there are OlderCount older versions.

pack_latest_version(Pack, SHA1, Version, Older) :-
	setof(SHA1, sha1_pack(SHA1, Pack), Hashes),
	map_list_to_pairs(sha1_version, Hashes, Versions),
	keysort(Versions, Sorted),
	length(Sorted, Count),
	Older is Count - 1,
	last(Sorted, Version-SHA1).

pack_downloads(Pack, SHA1) -->
	{ setof(Hash, sha1_pack(Hash, Pack), Hashes),
	  map_list_to_pairs(sha1_downloads, Hashes, Pairs),
	  memberchk(DownLoadLatest-SHA1, Pairs),
	  pairs_keys(Pairs, Counts),
	  sum_list(Counts, Total)
	},
	html('~D (~D)'-[Total, DownLoadLatest]).

pack_title(SHA1) -->
	{ sha1_title(SHA1, Title)
	},
	html(Title).

%%	pack_info(+Pack)//
%
%	Provided detailed information about a package.
%
%	@tbd	provide many more details
%	@tbd	Show dependency for requirements/provides

pack_info(Pack) -->
	{ pack_latest_version(Pack, SHA1, Version, _Older),
	  prolog_pack:atom_version(VersionA, Version),
	  sha1_title(SHA1, Title),
	  sha1_info(SHA1, Info)
	},
	html(table(class(pack),
		   [ \property('Title', Title),
		     \property('Latest version', VersionA),
		     \property('SHA1 sum', SHA1),
		     \info(author(_,_), Info),
		     \info(maintainer(_,_), Info),
		     \info(packager(_,_), Info),
		     \info(home(_), Info),
		     \info(download(_), Info),
		     \info(requires(_), Info),
		     \info(provides(_), Info),
		     \info(conflicts(_), Info)
		   ])).

property(Label, Value) -->
	html(tr([th([Label, :]), td(Value)])).

info(Term, Info) -->
	{ findall(Term, member(Term, Info), [T0|More]), !,
	  prolog_pack:pack_level_info(_, Term, LabelFmt, _),
	  (   LabelFmt = Label-Fmt
	  ->  true
	  ;   Label = LabelFmt, Fmt = '~w'
	  ),
	  T0 =.. [_|Values]
	},
	html(tr([th([Label, :]), td(Fmt-Values)])),
	extra_values(More, Fmt).
info(_, _) --> [].

extra_values([], _) --> [].
extra_values([H|T], Fmt) -->
	{ H =.. [_|Values] },
	html(tr([th([]), td(Fmt-Values)])),
	extra_values(T, Fmt).


