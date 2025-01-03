/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013-2024, VU University Amsterdam
                              SWI-Prolog Solutions b.v.

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

:- module(pack,
          [ pack/1,                     % ?Pack
            pack_version_hashes/2,      % +Pack, -VersionHashesPairs
            hash_git_url/2,             % +Hash, -URL
            hash_file_url/2,            % +Hash, -URL
            pack_url_hash/2,            % +URL, -SHA1

            current_pack/2,             % +Filter, -Pack
            sort_packs/3,               % +By, +Packs, -Sorted
            pack_table//2               % +Packs, +Options
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(persistency)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(library(option)).
:- use_module(library(record)).
:- use_module(library(pairs)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(prolog_versions)).

:- use_module(pack_info).
:- use_module(pack_mirror).
:- use_module(review).
:- use_module(messages).
:- use_module(openid).
:- use_module(proxy).
:- use_module(parms).

:- http_handler(root(pack/query),        pack_query,        []).
:- http_handler(root(pack/list),         pack_list,         [prefix]).
:- http_handler(root(pack/file_details), pack_file_details,
                [prefix, time_limit(20)]).
:- http_handler(root(pack/delete),       pack_delete,       []).
:- http_handler(root(pack/pattern),      set_allowed_url,   []).

%!  pack_query(+Request)
%
%   Handle package query requests from remote installers.  Content
%   is of type application/x-prolog.   Reply is also a Prolog term.

pack_query(Request) :-
    proxy_master(Request),
    !.
pack_query(Request) :-
    memberchk(content_type(ContentType), Request),
    content_x_prolog(ContentType, ReplyType),
    !,
    http_peer(Request, Peer),
    http_read_data(Request, Query,
                   [ content_type('application/x-prolog')
                   ]),
    http_log('pack_query(~q, ~q).~n', [Query, Peer]),
    format('Cache-Control: private~n'),
    (   catch(pack_query(Query, Peer, Reply), E, true)
    ->  format('Content-type: ~w; charset=UTF-8~n~n', [ReplyType]),
        (   var(E)
        ->  format('~q.~n', [true(Reply)]),
            http_log('pack_query_done(ok, ~q).~n', [Peer])
        ;   format('~q.~n', [exception(E)]),
            message_to_string(E, String),
            http_log('pack_query_done(error(~q), ~q).~n', [String, Peer])
        )
    ;   format('Content-type: ~w; charset=UTF-8~n~n', [ReplyType]),
        format('false.~n'),
        http_log('pack_query_done(failed, ~q).~n', [Peer])
    ).

content_x_prolog(ContentType, 'text/x-prolog') :-
    sub_atom(ContentType, 0, _, _, 'text/x-prolog'),
    !.
content_x_prolog(ContentType, 'application/x-prolog') :-
    sub_atom(ContentType, 0, _, _, 'application/x-prolog').

%!  proxy_master(Request)
%
%   Proxy the request to the master to make sure the central package
%   database remains synchronised.

proxy_master(Request) :-
    option(host(Host), Request),
    server(Role, Host),
    Role \== master,
    server(master, Master),
    Master \== Host,
    !,
    http_peer(Request, Peer),
    format(string(To), 'https://~w', [Master]),
    proxy(To, Request,
          [ request_headers([ 'X-Forwarded-For' = Peer,
                              'X-Real-IP' = Peer,
                              'Cache-Control' = 'no-cache'
                            ])
          ]).


%!  pack_query(+Query, +Peer, -Reply) is det.
%
%   Implements  the  various  queries    from   the  pack_install/1.
%   Currently defined Query values are:
%
%     * install(+URL, +SHA1, +Info)
%     User tries to install from URL an object with the indicated
%     hash and Info.
%     * downloaded(+Data)
%     Register download for indicated Data
%     * locate(+Pack)
%     Query download locations for Pack.
%     * versions(+Packs, +Options)
%     Query download and versions for a set of packs and all
%     (recursive) dependencies.
%     * search(+Keyword)
%     Find packs that match Keyword.
%     * info(+Packs)
%     Return a list of meta-data terms for the latest version of
%     Packs.  Unknown packs are omitted from the result list.

pack_query(install(URL0, SHA10, Info), Peer, Reply) =>
    to_atom(URL0, URL),
    to_atom(SHA10, SHA1),
    save_request(Peer, download(URL, SHA1, Info), Result),
    (   Result = throw(Error)
    ->  throw(Error)
    ;   findall(ReplyInfo, install_info(URL, SHA1, ReplyInfo), Reply)
    ).
pack_query(downloaded(Data), Peer, Reply) =>
    maplist(save_request(Peer), Data, Reply).
pack_query(locate(Pack), _, Reply) =>
    pack_version_urls_v1(Pack, Reply).
pack_query(versions(Pack, Options), _, Reply) =>
    pack_versions(Pack, Reply, Options).
pack_query(search(Word), _, Reply) =>
    search_packs(Word, Reply).
pack_query(info(Packs), _, Hits) =>
    convlist(pack_search_result, Packs, Hits).

to_atom(Atom, Atom) :-
    atom(Atom),
    !.
to_atom(String, Atom) :-
    atom_string(Atom, String).

%!  pack_admin(+Pack)//
%
%   Display pack admin options

pack_admin(Pack) -->
    { admin_user },
    !,
    html(div(class('pack-admin'),
             [ div(class('delete-pack'), \delete_button(Pack)),
               div(style('clear:right'), \pattern_input(Pack))
             ])).
pack_admin(_) -->
    [].

delete_button(Pack) -->
    { http_link_to_id(pack_delete, [], HREF)
    },
    html(form([ action(HREF),
                class('delete-pack')
              ],
              [ input([ type(hidden), name(p), value(Pack)]),
                button([type(submit)], 'Delete pack'),
                &(nbsp)
              ])).

pattern_input(Pack) -->
    { http_link_to_id(set_allowed_url, [], HREF),
      (   pack_allowed_url(Pack, IsGit, Pattern)
      ->  true
      ;   pack_version_hashes(Pack, VersionHashes),
          member(_-Hashes, VersionHashes),
          member(Hash, Hashes),
          sha1_url(Hash, URL)
      ->  url_pattern(URL, IsGit, Pattern)
      ;   Pattern = "",
          IsGit = false
      )
    },
    html(form([ action(HREF),
                class('pack-set-url-pattern')
              ],
              [ input([ type(hidden), name(p), value(Pack)]),
                label(for(url), 'URL pattern'),
                input([ class('url-pattern'), name(url), value(Pattern)]),
                input([ type(checkbox), name(git), value(IsGit)]),
                label(for(git), 'Is GIT'),
                button([type(submit)],
                       'Update URL pattern'),
                &(nbsp)
              ])).


admin_user :-
    current_prolog_flag(admin, true),
    !.
admin_user :-
    site_user_logged_in(User),
    site_user_property(User, granted(admin)).

%!  pack_delete(+Request)
%
%   HTTP handler to delete a pack

pack_delete(Request) :-
    admin_user,
    http_parameters(Request,
                    [ p(Pack, [optional(true)]),
                      h(Hash, [optional(true)])
                    ], []),
    (   nonvar(Pack)
    ->  call_showing_messages(delete_pack(Pack), [])
    ;   nonvar(Hash)
    ->  call_showing_messages(delete_hash(Hash), [])
    ).
pack_delete(Request) :-
    memberchk(path(Path), Request),
    throw(http_reply(forbidden(Path))).

                 /*******************************
                 *      COMPUTATIONAL LOGIC     *
                 *******************************/

%!  install_info(+URL, +SHA1, -Info) is nondet.
%
%   Info is relevant information  for  the   client  who  whishes to
%   install URL, which has the given   SHA1 hash. Currently provided
%   info is:
%
%     - alt_hash(Downloads, URLs, Hash)
%       Another file with the same (base) name was registered that
%       has a different hash.  This file was downloaded Downloads
%       times, resides on the given URLs (a list) and has the given
%       Hash.
%     - downloads(Downloads)
%       This hash was downloaded Downloads times from a unique IP
%       address
%     - dependency(Token, Pack, Version, URLs, SubSeps)
%       The requirement Token can be provided by Pack@Version, which
%       may be downloaded from the given URLs (a list).  Pack has
%       install info as specified by SubSeps (recursive
%       dependencies)

install_info(URL, SHA1, Info) :-
    install_info(URL, SHA1, Info, []).

install_info(_, SHA1, _, Seen) :-
    memberchk(SHA1, Seen), !, fail.
install_info(URL, SHA1, alt_hash(Downloads, URLs, Hash), _) :-
    prolog_pack:pack_url_file(URL, File),
    sha1_file(Hash, File),
    Hash \== SHA1,
    \+ is_github_release(URL),
    sha1_downloads(Hash, Downloads),
    sha1_urls(Hash, URLs).
install_info(_, SHA1, downloads(Count), _) :-
    sha1_downloads(SHA1, Count).
install_info(_, SHA1, dependency(Token, Pack, Version, URLs, SubDeps), Seen) :-
    sha1_requires(SHA1, Token),
    \+ is_prolog_token(Token),      % not in this version
    (   (   sha1_pack(_Hash, Token),
            Pack = Token
        ;   sha1_provides(Hash, Token),
            sha1_pack(Hash, Pack),
            Pack \== Token
        ),
        pack_latest_version(Pack, Hash1, _VersionTerm, _Older),
        sha1_info(Hash1, Info),
        memberchk(version(Version), Info),
        findall(URL, sha1_url(Hash1, URL), URLs),
        URLs \== []
    ->  findall(SubDep, install_info(-, Hash1, SubDep, [SHA1|Seen]), SubDeps)
    ;   Pack = (-), Version = (-), URLs = []
    ).

%!  is_prolog_token(+Token) is semidet.
%
%   @tbd: share with library(pack_install).

is_prolog_token(Token), cmp(Token, prolog, _Cmp, _Version) => true.
is_prolog_token(prolog:_Feature) => true.
is_prolog_token(_) => fail.

sha1_downloads(Hash, Count) :-
    aggregate_all(count, sha1_download(Hash, _), Count).

sha1_urls(Hash, URLs) :-
    findall(URL, sha1_url(Hash, URL), URLs).

sha1_version(Hash, Version) :-
    sha1_info(Hash, Info),
    memberchk(version(Atom), Info),
    atom_version(Atom, Version).

sha1_title(Hash, Title) :-
    sha1_info(Hash, Info),
    (   memberchk(title(Title), Info)
    ->  true
    ;   Title = '<no title>'
    ).

sha1_is_git(Hash, Boolean) :-
    sha1_info(Hash, Info),
    (   memberchk(git(true), Info)
    ->  Boolean = true
    ;   Boolean = false
    ).


%!  pack_version_hashes(+Pack, -VersionHashesPairs) is semidet.
%
%   True when HashesByVersion is  an   ordered  list Version-Hashes,
%   latest version first.

pack_version_hashes(Pack, VersionAHashesPairs) :-
    findall(SHA1, sha1_pack(SHA1, Pack), Hashes),
    map_list_to_pairs(sha1_version, Hashes, VersionHashPairs),
    keysort(VersionHashPairs, Sorted),
    group_pairs_by_key(Sorted, VersionHashesPairs),
    reverse(VersionHashesPairs, RevPairs),
    maplist(atomic_version_hashes, RevPairs, VersionAHashesPairs).

atomic_version_hashes(Version-Hashes, VersionA-Hashes) :-
    atom_version(VersionA, Version).

%!  pack_version_urls_v1(+Pack, -Locations) is det.
%
%   True when Locations is a set of Version-list(URL) pairs used for
%   installing Pack.
%
%   @arg    Locations is a list Version-URLs, sorted latest version
%           first.
%   @see    pack_version_urls_v2/3

pack_version_urls_v1(Pack, VersionURLs) :-
    pack_version_hashes(Pack, VersionHashes),
    maplist(version_hashes_urls, VersionHashes, VersionURLs).

version_hashes_urls(Version-Hashes, Version-URLs) :-
    maplist(sha1_url, Hashes, URLs0),
    sort(URLs0, URLs).

%!  pack_versions(+Packs, -PackVersions, +Options) is det.
%
%   Given a single or multiple  packs,   return  information  on all
%   these packs as well as  their   dependencies.  PackVersions is a
%   list   of   `Pack-Versions`.   `Versions`   is     a   list   of
%   `Version-InfoList`. `InfoList` is a list of dicts, each holding
%
%      - info.pack
%        Pack name
%      - info.hash
%        Hash of the version.   This is either a GIT hash or the
%        sha1 of the archive file.
%      - info.provides
%        List of provided tokens.  Each provide is either a simple
%        token or a term @(Token,Version).
%      - info.requires
%        List of required tokens.  Each requirement is either a
%        simple token or a term `Token cmp Version`, where _cmp_
%        is one of `<`, `=<`, `=`, `>=` or `>`.
%      - info.conflicts
%        Similar to `info.requires`, declaring conflicts
%      - info.url
%        URL for downloading the archive or URL of the git repo.
%      - info.git
%        Boolean expressing wether the URL is a git repo or
%        archive.
%      - info.downloads
%        Download count.

pack_versions(Packs, Deps, Options) :-
    phrase(pack_versions(Packs, [seen(Deps)|Options]), Deps).

pack_versions([], _) --> !.
pack_versions([H|T], Options) -->
    pack_versions(H, Options),
    pack_versions(T, Options).
pack_versions(Pack, Options) -->
    { option(seen(Deps), Options),
      seen(Pack, Deps)
    },
    !.
pack_versions(Pack, Options) -->
    { pack_version_hashes(Pack, VersionHashes),
      convlist(version_hash_info(Pack, Options),
               VersionHashes, Infos),
      maplist(arg(2), Infos, RequiresLists),
      append(RequiresLists, Requires0),
      sort(Requires0, Requires),
      maplist(arg(1), Infos, VersionInfo)
    },
    [ Pack-VersionInfo ],
    include_pack_requirements(Requires, Options).

seen(Pack, [Pack-_|_]) => true.
seen(Pack, [_|T]) => seen(Pack, T).
seen(_, _) => fail.

version_hash_info(Pack, Options, Version-Hashes, info(Version-Info, Requires)) :-
    maplist(hash_info(Pack, Options), Hashes, Info, Requires0),
    append(Requires0, Requires1),
    sort(Requires1, Requires).

hash_info(Pack, _Options, Hash, Dict, Requires) :-
    sha1_url(Hash, URL),
    sha1_is_git(Hash, IsGit),
    sha1_downloads(Hash, Count),
    findall(Req, sha1_requires(Hash, Req), Requires),
    findall(Prv, sha1_provides(Hash, Prv), Provides),
    findall(Prv, sha1_conflicts(Hash, Prv), Conflicts),
    Dict = #{ pack: Pack,
              hash: Hash,
              url: URL,
              git: IsGit,
              requires: Requires,
              provides: Provides,
              conflicts: Conflicts,
              downloads: Count
            }.

include_pack_requirements([], _) --> !.
include_pack_requirements([ReqToken|T], Options) -->
    { findall(Unseen, resolves(ReqToken, Unseen), DepPacks)
    },
    pack_versions(DepPacks, Options),
    include_pack_requirements(T, Options).

resolves(ReqToken, Pack) :-
    (   sha1_pack(Hash, Token),
        sha1_version(Hash, Version),
        PrvToken = @(Token,Version)
    ;   sha1_provides(Hash, PrvToken)
    ),
    satisfies(PrvToken, ReqToken),
    sha1_pack(Hash, Pack).

satisfies(Token, Token) => true.
satisfies(@(Token,_), Token) => true.
satisfies(@(Token,PrvVersion), Req), cmp(Req, Token, Cmp, ReqVersion) =>
    atomic_list_concat(PrvVersion, PrvVersionAtom),
    atomic_list_concat(ReqVersion, ReqVersionAtom),
    cmp_versions(Cmp, PrvVersionAtom, ReqVersionAtom).
satisfies(_,_) => fail.

cmp(Token  < Version, Token, <,  Version).
cmp(Token =< Version, Token, =<, Version).
cmp(Token =  Version, Token, =,  Version).
cmp(Token == Version, Token, ==, Version).
cmp(Token >= Version, Token, >=, Version).
cmp(Token >  Version, Token, >,  Version).

%!  search_packs(+Search, -Packs) is det.
%
%   Search packs by keyword, returning a list
%
%           pack(Pack, Status, Version, Title, URLs).

search_packs(Search, Packs) :-
    setof(Pack, matching_pack(Search, Pack), Names),
    !,
    maplist(pack_search_result, Names, Packs).

matching_pack(Search, Pack) :-
    sha1_pack(SHA1, Pack),
    (   sub_atom_icasechk(Pack, _, Search)
    ->  true
    ;   sha1_title(SHA1, Title),
        sub_atom_icasechk(Title, _, Search)
    ).

pack_search_result(Pack, pack(Pack, p, Title, VersionA, URLs)) :-
    pack_latest_version(Pack, SHA1, Version, _Older),
    sha1_title(SHA1, Title),
    atom_version(VersionA, Version),
    findall(URL, sha1_url(SHA1, URL), URLs).


                 /*******************************
                 *           DATABASE           *
                 *******************************/

:- multifile error:has_type/2.

error:has_type(dependency, Value) :-
    is_dependency(Value, _Token, _Version).

is_dependency(Token, Token, *) :-
    atom(Token).
is_dependency(Term, Token, VersionCmp) :-
    Term =.. [Op,Token,Version],
    cmp(Op, _),
    version_data(Version, _),
    VersionCmp =.. [Op,Version].

cmp(<,  @<).
cmp(=<, @=<).
cmp(==, ==).
cmp(=,  =).
cmp(>=, @>=).
cmp(>,  @>).

version_data(Version, version(Data)) :-
    atomic_list_concat(Parts, '.', Version),
    maplist(atom_number, Parts, Data).

:- persistent
    sha1_pack(sha1:atom, pack:atom),
    sha1_file(sha1:atom, file:atom),
    sha1_requires(sha1:atom, token:dependency),
    sha1_provides(sha1:atom, token:dependency),
    sha1_conflicts(sha1:atom, token:dependency),
    sha1_info(sha1:atom, info:list),
    sha1_url(sha1:atom, url:atom),
    sha1_download(sha1:atom, peer:atom),
    pack_allowed_url(pack:atom, isgit:boolean, pattern:atom).

:- initialization
    absolute_file_name(data('packs.db'), File,
                       [ access(write) ]),
    db_attach(File, [sync(close)]),
    populate_pack_url_patterns.

%!  delete_pack(+PackName) is det.
%
%   Remove a pack from the database.

delete_pack(PackName) :-
    must_be(atom, PackName),
    pack(PackName),
    !,
    clean_pack_info(PackName),
    pack_unmirror(PackName),
    forall(sha1_pack(Hash, PackName),
           delete_hash(Hash)),
    retractall_pack_allowed_url(PackName,_,_),
    print_message(informational, delete_pack(PackName)).
delete_pack(PackName) :-
    existence_error(pack, PackName).

%!  delete_hash(Hash) is det.
%
%   Remove Hash from the database

delete_hash(Hash) :-
    retractall_sha1_pack(Hash, _),
    retractall_sha1_file(Hash, _),
    retractall_sha1_requires(Hash, _),
    retractall_sha1_provides(Hash, _),
    retractall_sha1_conflicts(Hash, _),
    retractall_sha1_info(Hash, _),
    retractall_sha1_url(Hash, _),
    retractall_sha1_download(Hash, _),
    print_message(informational, delete_hash(Hash)).

%!  save_request(+Peer, +Data, -Result)
%
%   Update the database with the given   information. We only update
%   if the request is new, which means   the  same SHA1 has not been
%   downloaded from the same Peer.

:- det(save_request/3).
save_request(Peer, download(URL, Hash, Metadata), Result) =>
    Result = Pack-Action,
    memberchk(name(Pack), Metadata),
    with_mutex(pack, save_request(URL, Hash, Metadata, Peer, Action)).

save_request(URL, Hash, Metadata, Peer, Result) :-
    (   Error = error(Formal,_),
        catch(save_request_(URL, Hash, Metadata, Peer, Res0),
              Error,
              true)
    ->  (   var(Formal)
        ->  Result = Res0
        ;   Result = throw(Error)
        )
    ;   Result = false
    ).

save_request_(URL, SHA1, Info, Peer, Result) :-
    sha1_download(SHA1, Peer),
    sha1_pack(SHA1, Peer),                 % already downloaded from here
    !,
    info_is_git(Info, IsGIT),
    register_url(SHA1, IsGIT, URL, Result). % but maybe from a different URL
save_request_(URL, SHA1, Info, Peer, Result) :-
    memberchk(name(Pack), Info),
    info_is_git(Info, IsGIT),
    (   accept_url(URL, Pack, IsGIT)
    ->  register_url(SHA1, IsGIT, URL, Result0),
        register_pack(SHA1, Pack),
        register_info(SHA1, Info)
    ;   permission_error(register, pack(Pack), URL)
    ),
    assert_sha1_download(SHA1, Peer),
    (   Result0 == no_change
    ->  Result = download
    ;   Result = Result0
    ).

info_is_git(Info, IsGIT) :-
    memberchk(git(IsGIT), Info),
    !.
info_is_git(_, false).

%!  accept_url(+URL, +Pack, +IsGit) is det.
%
%   True when URL is an aceptable URL for Pack.  We only
%   register this on the first submission of a pack.

accept_url(URL, Pack, IsGIT) :-
    (   pack_allowed_url(Pack, _, Pattern)
    *-> wildcard_match(Pattern, URL), !
    ;   admissible_url(URL)
    ->  url_pattern(URL, IsGIT, Pattern),
        assert_pack_allowed_url(Pack, IsGIT, Pattern)
    ).

admissible_url(URL) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    uri_data(authority, Components, Authority),
    uri_authority_components(Authority, AuthComponents),
    uri_authority_data(host, AuthComponents, Host),
    uri_authority_data(port, AuthComponents, Port),
    \+ nonadmissible_host(Host),
    admissible_scheme(Scheme, Port).

nonadmissible_host(localhost).
nonadmissible_host(IP) :-
    split_string(IP, ".", "", Parts),
    maplist(number_string, _, Parts).

admissible_scheme(http, 80).
admissible_scheme(https, 443).

url_pattern(URL, true, URL) :- !.
url_pattern(URL, false, Pattern) :-
    site_pattern(URL, Pattern),
    !.
url_pattern(URL, false, Pattern) :-
    (   atom_concat('http://', Rest, URL)
    ->  atom_concat('http{,s}://', Rest, URL2)
    ;   URL2 = URL
    ),
    file_directory_name(URL2, Dir),
    atom_concat(Dir, '/*', Pattern).

site_pattern(URL, Pattern) :-
    sub_atom(URL, 0, _, _, 'https://gitlab.com/'),
    git_user_project_pattern(URL, Pattern).
site_pattern(URL, Pattern) :-
    sub_atom(URL, 0, _, _, 'https://github.com/'),
    git_user_project_pattern(URL, Pattern).

git_user_project_pattern(URL, Pattern) :-
    uri_components(URL, Components),
    uri_data(path, Components, Path0),
    split_string(Path0, "/", "/", [User,Project|_]),
    atomic_list_concat([/, User, /, Project, /, *], Path),
    uri_data(path, Components, Path, Components1),
    uri_components(Pattern, Components1).

populate_pack_url_patterns :-
    forall(pack(Pack),
           populate_pack_url_pattern(Pack)).

populate_pack_url_pattern(Pack) :-
    pack_allowed_url(Pack, _, _),
    !.
populate_pack_url_pattern(Pack) :-
    findall(URL-IsGIT,
            ( sha1_pack(SHA1, Pack),
              sha1_info(SHA1, Info),
              (   memberchk(git(IsGIT), Info)
              ->  true
              ;   IsGIT = false
              ),
              sha1_url(SHA1, URL)
            ),
            URLS),
    last(URLS, URL-IsGIT),
    url_pattern(URL, IsGIT, Pattern),
    assert_pack_allowed_url(Pack, IsGIT, Pattern),
    !.
populate_pack_url_pattern(Pack) :-
    print_message(error, pack(pattern_failed(Pack))).

%!  set_allowed_url(+Request)
%
%   Set the URL pattern for a pack.

set_allowed_url(Request) :-
    admin_user,
    http_parameters(Request,
                    [ p(Pack, []),
                      url(Pattern, []),
                      git(IsGit, [boolean, optional(true)])
                    ], []),
    call_showing_messages(set_allowed_url(Pack, IsGit, Pattern), []).
set_allowed_url(Request) :-
    memberchk(path(Path), Request),
    throw(http_reply(forbidden(Path))).

set_allowed_url(Pack, _IsGit, _Pattern) :-
    \+ sha1_pack(_, Pack),
    !,
    existence_error(pack, Pack).
set_allowed_url(Pack, IsGit, Pattern) :-
    (   var(IsGit)
    ->  (   sub_atom(Pattern, _, _, _, *)
        ->  IsGit = false
        ;   IsGit = true
        )
    ;   true
    ),
    retractall_pack_allowed_url(Pack, _, _),
    assert_pack_allowed_url(Pack, IsGit, Pattern).

%!  register_pack(+SHA1, +Pack) is det.

register_pack(SHA1, Pack) :-
    (   sha1_pack(SHA1, Pack)
    ->  true
    ;   assert_sha1_pack(SHA1, Pack)
    ).

register_info(SHA1, Info0) :-
    sort(Info0, Info),
    (   sha1_info(SHA1, _Info)
    ->  true
    ;   assert_sha1_info(SHA1, Info),
        forall(member(requires(Token), Info),
               register_requires(SHA1, Token)),
        forall(member(provides(Token), Info),
               register_provides(SHA1, Token)),
        forall(member(conflicts(Token), Info),
               register_conflicts(SHA1, Token))
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

register_conflicts(SHA1, Token) :-
    (   sha1_conflicts(SHA1, Token)
    ->  true
    ;   assert_sha1_conflicts(SHA1, Token)
    ).

%!  register_url(+SHA1, +IsGIT, +URL) is det.
%
%   Register we have that data loaded from URL has signature SHA1.

:- debug(pack(changed)).

register_url(SHA1, IsGIT, URL, Result) :-
    (   sha1_url(SHA1, URL)
    ->  Result = no_change
    ;   sha1_url(SHA2, URL),
        \+ ( IsGIT == true,
             hash_git_url(SHA2, URL)
           ),
        (   debug(pack(changed), '~p seems changed', [URL]),
            is_github_release(URL)
        ->  debug(pack(changed), 'From github: ~p', [URL]),
            retractall_sha1_url(SHA1, URL),
            fail
        ;   true
        )
    ->  Result = throw(pack(modified_hash(SHA1-URL, SHA2-[URL])))
    ;   IsGIT == true
    ->  assert_sha1_url(SHA1, URL),
        Result = git(URL)
    ;   prolog_pack:pack_url_file(URL, File),
        register_file(SHA1, File, URL),
        assert_sha1_url(SHA1, URL),
        Result = file(URL)
    ).

%!  is_github_release(+URL) is semidet.
%
%   True when URL reflects a  GitHub   release  pack download. These
%   have the unpeleasant habbit to change exact content.

is_github_release(URL) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme), Scheme == https,
    uri_data(authority, Components, Auth), Auth == 'github.com',
    uri_data(path, Components, Path), atomic(Path),
    split_string(Path, "/", "", ["", _User, _Repo, "archive", Zip]),
    file_name_extension(_, Ext, Zip),
    github_archive_extension(Ext).

github_archive_extension(tgz).
github_archive_extension(zip).

register_file(SHA1, File, URL) :-
    (   sha1_file(SHA1, File)
    ->  true
    ;   sha1_file(SHA2, File),
        sha1_urls(SHA2, URLs),
        (   maplist(is_github_release, [URL|URLs])
        ->  retractall_sha1_file(SHA1, File),
            fail
        ;   true
        )
    ->  throw(pack(modified_hash(SHA1-URL, SHA2-URLs)))
    ;   assert_sha1_file(SHA1, File)
    ).

%!  hash_git_url(+SHA1, -GitURL) is semidet.
%
%   True when SHA1 was installed using GIT from GitURL.

hash_git_url(SHA1, GitURL) :-
    sha1_info(SHA1, Info),
    memberchk(git(true), Info),
    !,
    sha1_url(SHA1, GitURL).

%!  hash_file_url(+SHA1, -FileURL) is nondet.
%
%   True when SHA1 was installed using GIT from GitURL.

hash_file_url(SHA1, FileURL) :-
    sha1_info(SHA1, Info),
    \+ memberchk(git(true), Info),
    !,
    sha1_url(SHA1, FileURL).

%!  pack_url_hash(?URL, ?Hash) is nondet.
%
%   True when Hash is the registered hash for URL.

pack_url_hash(URL, Hash) :-
    sha1_url(Hash, URL).

%!  pack(?Pack) is nondet.
%
%   True when Pack is a currently known pack.

pack(Pack) :-
    findall(Pack, sha1_pack(_,Pack), Packs),
    sort(Packs, Sorted),
    member(Pack, Sorted).


                 /*******************************
                 *           USER API           *
                 *******************************/

%!  pack_list(+Request)
%
%   List available packages.

pack_list(Request) :-
    memberchk(path_info(SlashPack), Request),
    atom_concat(/, Pack, SlashPack),
    format(atom(Title), '"~w" pack for SWI-Prolog', [Pack]),
    reply_html_page(pack(list),
                    title(Title),
                    [ \pack_listing(Pack, _Author, _Sort)
                    ]).
pack_list(Request) :-
    http_parameters(Request,
                    [ p(Pack, [optional(true)]),
                      author(Author, [optional(true)]),
                      sort(Sort, [ oneof([name,downloads,rating]),
                                   optional(true),
                                   default(name)
                                 ])
                    ]),
    (  ground(Pack)
    -> format(atom(Title), '"~w" pack for SWI-Prolog', [Pack])
    ;  Title = 'SWI-Prolog packages'
    ),
    reply_html_page(pack(list),
                    title(Title),
                    [ \pack_listing(Pack, Author, Sort)
                    ]).

pack_listing(Pack, _Author, _Sort) -->
    { ground(Pack) },
    !,
    html([ h1(class(wiki), 'Package "~w"'-[Pack]),
           \html_requires(css('pack.css')),
           \pack_info(Pack)
         ]).
pack_listing(_Pack, Author, SortBy) -->
    { (   nonvar(Author)
      ->  Filter = [author(Author)]
      ;   Filter = []
      ),
      (   setof(Pack, current_pack(Filter, Pack), Packs)
      ->  true
      ;   Packs = []
      ),
      sort_packs(SortBy, Packs, Sorted)
    },
    html({|html||
<p>
Below is a list of known packages. Please be aware that packages are
<b>not moderated</b>. Installing a pack does not execute code in the
pack, but simply loading a library from the pack may execute arbitrary
code. More information about packages is available <a
href="/howto/Pack.html">here</a>.   You can search for packages from
the Prolog command line using pack_list/1.  This contacts the pack
server for packs that match by name or title.  A leading <b>i</b>
indicates that the pack is already installed, while <b>p</b> merely
indicates that it is known by the server.
</p>

<pre class="code">
?- pack_list(graph).
p callgraph@0.3.4           - Predicate call graph visualisation
i graphml@0.1.0             - Write GraphML files
i gvterm@1.1                - Show Prolog terms using graphviz
p musicbrainz@0.6.3         - Musicbrainz client library
p sindice@0.0.3             - Access to Sindice semantic web search engine
</pre>

<p>
After finding the right pack, the pack and its dependencies can be installed
using the pack_install/1 as illustrated below.
</p>

<pre class="code">
?- pack_install(hello).
</pre>

<p>
Clicking the package shows details and allows you to rate and comment
the pack.
</p>
             |}),
    pack_table(Sorted, [sort_by(SortBy)]),
    html_receive(rating_scripts).

%!  pack_table(+Packs, +Options)// is det.
%
%   Show a table of packs.

pack_table(Packs, Options) -->
    { option(sort_by(SortBy), Options, -),
      length(Packs, PackCount),
      maplist(pack_downloads, Packs, Totals),
      sum_list(Totals, Total)
    },
    html_requires(css('pack.css')),
    html(table(class(packlist),
               [ tr([ \pack_header(name,  SortBy,
                                   'Pack', ['tot: ~D'-[PackCount]]),
                      \pack_header(version, SortBy,
                                   'Version', '(#older)'),
                      \pack_header(downloads, SortBy,
                                   'Downloads', ['tot: ~D'-[Total],
                                                 br([]), '(#latest)']),
                      \pack_header(rating, SortBy,
                                   'Rating', ['(#votes/', br([]),
                                              '#comments)']),
                      \pack_header(title, SortBy,
                                   'Title', [])
                    ])
               | \pack_rows(Packs)
               ])).


pack_rows([]) --> [].
pack_rows([H|T]) --> pack_row(H), pack_rows(T).

pack_row(Pack) -->
    { pack_name(Pack, Name),
      http_link_to_id(pack_list, [p(Name)], HREF)
    },
    html(tr([ td(a(href(HREF),Name)),
              td(class('pack-version'),   \pack_version(Pack)),
              td(class('pack-downloads'), \pack_downloads(Pack)),
              td(class('pack-rating'),    \pack_rating(Pack)),
              td(class('pack-title'),     \pack_title(Pack))
            ])).

pack_header(Name, -, Title, Subtitle) -->
    !,
    html(th(id(Name), [Title, \subtitle(Subtitle)])).
pack_header(Name, SortBy, Title, Subtitle) -->
    { Name \== SortBy,
      sortable(Name),
      !,
      http_link_to_id(pack_list, [sort(Name)], HREF)
    },
    html(th(id(Name), [ a([class(resort),href(HREF)], Title),
                        \subtitle(Subtitle)
                      ])).
pack_header(Name, Name, Title, Subtitle) -->
    html(th(id(Name), [i(class(sorted), Title), \subtitle(Subtitle)])).
pack_header(Name, _, Title, Subtitle) -->
    html(th(id(Name), [Title, \subtitle(Subtitle)])).

subtitle([]) --> [].
subtitle(Subtitle) --> html(div(class(sth), Subtitle)).


sortable(name).
sortable(downloads).
sortable(rating).

pack_version(Pack) -->
    { pack_version(Pack, Version),
      pack_older_versions(Pack, Older),
      atom_version(Atom, Version)
    },
    (   { Older =\= 0 }
    ->  html([Atom, span(class(annot), '~D'-[Older])])
    ;   html(Atom)
    ).

pack_downloads(Pack) -->
    { pack_downloads(Pack, Total),
      pack_download_latest(Pack, DownLoadLatest)
    },
    (   { Total =:= DownLoadLatest }
    ->  html('~D'-[Total])
    ;   html(['~D'-[Total], span(class(annot), '~D'-[DownLoadLatest])])
    ).

pack_rating(Pack) -->
    { pack_rating(Pack, Rating),
      pack_votes(Pack, Votes),
      pack_comments(Pack, CommentCount),
      pack_name(Pack, Name),
      http_link_to_id(pack_rating, [], OnRating)
    },
    show_pack_rating(Name, Rating, Votes, CommentCount,
                     [ on_rating(OnRating)
                     ]).

pack_title(Pack) -->
    { pack_hash(Pack, SHA1),
      sha1_title(SHA1, Title)
    },
    html(Title).

:- record
    pack(name:atom,                         % Name of the pack
         hash:atom,                         % SHA1 of latest version
         version:list(integer),             % Latest Version
         older_versions:integer,            % # older versions
         downloads:integer,                 % Total downloads
         download_latest:integer,           % # downloads latest version
         rating:number,                     % Average rating
         votes:integer,                     % Vote count
         comments:integer).                 % Comment count

%!  current_pack(+Filter:list, -Pack) is nondet.
%
%   True when Pack is a pack that satisfies Filter. Filter is a list
%   of filter expressions. Currently defined filters are:
%
%     * author(+Author)
%     Pack is claimed by this author.

current_pack(Filters,
             pack(Pack, SHA1,
                  Version, OlderVersionCount,
                  Downloads, DLLatest,
                  Rating, Votes, CommentCount)) :-
    setof(Pack, H^sha1_pack(H,Pack), Packs),
    member(Pack, Packs),
    pack_latest_version(Pack, SHA1, Version, OlderVersionCount),
    maplist(pack_filter(SHA1), Filters),
    pack_downloads(Pack, SHA1, Downloads, DLLatest),
    pack_rating_votes(Pack, Rating, Votes),
    pack_comment_count(Pack, CommentCount).

pack_filter(SHA1, author(Author)) :-
    sha1_info(SHA1, Info),
    member(author(Name, Contact), Info),
    once(author_match(Author, Name, Contact)).

author_match(Author, Author, _).                % Specified author
author_match(Author, _, Author).                % Specified contact
author_match(UUID, Name, Contact) :-            % Specified UUID
    (   site_user_property(UUID, name(Name))
    ;   site_user_property(UUID, email(Contact))
    ;   site_user_property(UUID, home_url(Contact))
    ).


%!  sort_packs(+Field, +Packs, -Sorted)

sort_packs(By, Packs, Sorted) :-
    map_list_to_pairs(pack_data(By), Packs, Keyed),
    keysort(Keyed, KeySorted),
    pairs_values(KeySorted, Sorted0),
    reverse_sort(By, Sorted0, Sorted).

reverse_sort(name, Packs, Packs) :- !.
reverse_sort(_, Packs, RevPacks) :-
    reverse(Packs, RevPacks).


pack_downloads(Pack, SHA1, Total, DownLoadLatest) :-
    setof(Hash, sha1_pack(Hash, Pack), Hashes),
    map_list_to_pairs(sha1_downloads, Hashes, Pairs),
    memberchk(DownLoadLatest-SHA1, Pairs),
    pairs_keys(Pairs, Counts),
    sum_list(Counts, Total).

%!  pack_latest_version(+Pack, -SHA1, -Version, -OlderCount)
%
%   True when SHA1 is the  latest  version   of  Pack  at  the given
%   Version and there are OlderCount older versions.

pack_latest_version(Pack, SHA1, Version, Older) :-
    setof(SHA1, sha1_pack(SHA1, Pack), Hashes),
    map_list_to_pairs(sha1_version, Hashes, Versions),
    keysort(Versions, Sorted),
    length(Sorted, Count),
    Older is Count - 1,
    last(Sorted, Version-SHA1).


                 /*******************************
                 *        DETAILED INFO         *
                 *******************************/

%!  pack_info(+Pack)//
%
%   Provided detailed information about a package.
%
%   @tbd    provide many more details
%   @tbd    Show dependency for requirements/provides

pack_info(Pack) -->
    { \+ pack(Pack) },
    !,
    html(p(class(warning),
           'Sorry, I know nothing about a pack named "~w"'-[Pack])).
pack_info(Pack) -->
    pack_admin(Pack),
    pack_info_table(Pack),
    pack_reviews(Pack),
    pack_file_table(Pack),
    ( pack_readme(Pack) -> [] ; [] ),
    (   pack_file_hierarchy(Pack)
    ->  []
    ;   html(p(class(warning), 'Failed to process pack'))
    ).

%!  pack_info_table(+Pack)// is det.
%
%   Provide basic information on the package

pack_info_table(Pack) -->
    { pack_latest_version(Pack, SHA1, Version, _Older),
      atom_version(VersionA, Version),
      sha1_title(SHA1, Title),
      sha1_info(SHA1, Info)
    },
    html(table(class(pack),
               [ \property('Title', span(class(title), Title)),
                 \property('Rating', \show_pack_rating(Pack)),
                 \property('Latest version', VersionA),
                 \property('SHA1 sum', \hash(SHA1)),
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
    { findall(Term, member(Term, Info), [T0|More]), !
    },
    html(tr([th([\label(T0), :]), td(\value(T0))])),
    extra_values(More).
info(_, _) --> [].

extra_values([]) --> [].
extra_values([H|T]) -->
    html(tr([th([]), td(\value(H))])),
    extra_values(T).

label(Term) -->
    { prolog_pack:pack_level_info(_, Term, LabelFmt, _),
      (   LabelFmt = Label-_
      ->  true
      ;   Label = LabelFmt
      )
    },
    html(Label).

value(Term) -->
    { name_address(Term, Name, Address) },
    !,
    html([span(class(name), Name), ' ']),
    address(Address).
value(Term) -->
    { url(Term, Label, URL) },
    html(a(href(URL), Label)).
value(Term) -->
    { prolog_pack:pack_level_info(_, Term, LabelFmt, _),
      (   LabelFmt = _-Fmt
      ->  true
      ;   Fmt = '~w'
      ),
      Term =.. [_|Values]
    },
    html(Fmt-Values).

address(Address) -->
    { sub_atom(Address, _, _, _, @) },
    !,
    html(['<', Address, '>']).
address(URL) -->
    html(a(href(URL), URL)).

name_address(author(    Name, Address), Name, Address).
name_address(maintainer(Name, Address), Name, Address).
name_address(packager(  Name, Address), Name, Address).

url(home(URL), URL, URL).
url(download(Pattern), Pattern, URL) :-
    (   wildcard_pattern(Pattern)
    ->  file_directory_name(Pattern, Dir),
        ensure_slash(Dir, URL)
    ;   URL = Pattern
    ).

wildcard_pattern(URL) :- sub_atom(URL, _, _, _, *).
wildcard_pattern(URL) :- sub_atom(URL, _, _, _, ?).

ensure_slash(Dir, DirS) :-
    (   sub_atom(Dir, _, _, 0, /)
    ->  DirS = Dir
    ;   atom_concat(Dir, /, DirS)
    ).

%!  pack_file_table(+Pack)// is det.
%
%   Provide a table with the files, sorted by version, providing
%   statistics on downloads.

pack_file_table(Pack) -->
    { findall(Version-Hash, pack_version_hash(Pack, Hash, Version), Pairs0),
      sort(1, @>=, Pairs0, Pairs),
      group_pairs_by_key(Pairs, Grouped)
    },
    html(h2(class(wiki), 'Details by download location')),
    html(table(class(pack_file_table),
               [ tr([th('Version'), th('SHA1'), th('#Downloads'), th('URL')])
               | \pack_file_rows(Grouped)
               ])).

pack_file_rows([]) --> [].
pack_file_rows([H|T]) --> pack_file_row(H), pack_file_rows(T).

pack_file_row(Version-[H0|Hashes]) -->
    { sha1_downloads(H0, Count),
      sha1_urls(H0, [URL|URLs])
    },
    html(tr([ td(\version(Version)),
              td(style('white-space: nowrap'), \hash(H0)),
              \count(Count),
              td(\download_url(URL))
            ])),
    alt_urls(URLs),
    alt_hashes(Hashes),
    !.
pack_file_row(_) -->
    [].

alt_urls([]) --> [].
alt_urls([H|T]) --> alt_url(H), alt_urls(T).

alt_url(H) -->
    html(tr([td(''), td(''), td(''), td(\download_url(H))])).

alt_hashes([]) --> [].
alt_hashes([H|T]) --> alt_hash(H), alt_hashes(T).

alt_hash(H) -->
    { sha1_downloads(H, Count),
      sha1_urls(H, [URL|URLs])
    },
    html(tr([td(''), td(\hash(H)), \count(Count), td(\download_url(URL))])),
    alt_urls(URLs).

hash(H)           --> html(span(class(hash), H)), del_hash_link(H).
download_url(URL) --> html(a(href(URL), URL)).
count(N)          --> html(td(class(count), N)).
version(V)        --> { atom_version(Atom, V) },
    html(Atom).

del_hash_link(Hash) -->
    { admin_user,
      !,
      http_link_to_id(pack_delete, [h=Hash], HREF)
    },
    !,
    html(a([class('delete-hash'), href(HREF)], '\U0001F5D1')).
del_hash_link(_) -->
    [].

pack_version_hash(Pack, Hash, Version) :-
    sha1_pack(Hash, Pack),
    sha1_version(Hash, Version).

%!  pack_file_details(+Request)
%
%   HTTP handler to provide details on a file in a pack

pack_file_details(Request) :-
    memberchk(path_info(SlashPackAndFile), Request),
    \+ sub_atom(SlashPackAndFile, _, _, _, '/../'),
    !,
    http_parameters(Request,
                    [ public_only(Public),
                      show(Show)
                    ],
                    [ attribute_declarations(pldoc_http:param)
                    ]),
    atom_concat(/, PackAndFile, SlashPackAndFile),
    sub_atom(PackAndFile, B, _, A, /),
    !,
    sub_atom(PackAndFile, 0, B, _, Pack),
    sub_atom(PackAndFile, _, A, 0, File),
    pack_file_details(Pack, File,
                      [ public_only(Public),
                        show(Show)
                      ]).


                 /*******************************
                 *        DB MAINTENANCE        *
                 *******************************/

%!  atom_version(?Atom, ?Version)
%
%   Translate   between   atomic   version   representation   and   term
%   representation.  The  term  representation  is  a  list  of  version
%   components as integers and can be compared using `@>`

atom_version(Atom, version(Parts)) :-
    (   atom(Atom)
    ->  split_string(Atom, ".", "", Parts0),
        maplist(valid_version_part, Parts0, Parts)
    ;   atomic_list_concat(Parts, '.', Atom)
    ).

valid_version_part(String, Num) :-
    number_string(Num, String),
    !.
valid_version_part("*", _).

                 /*******************************
                 *          MESSAGES            *
                 *******************************/

:- multifile prolog:message//1.

prolog:message(delete_pack(Pack)) -->
    [ 'Deleted pack ~p'-[Pack] ].
prolog:message(delete_hash(Hash)) -->
    [ 'Deleted hash ~p'-[Hash] ].
