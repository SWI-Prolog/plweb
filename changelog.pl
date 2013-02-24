:- module(changelog,
	  [
	  ]).
:- use_module(library(settings)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(wiki).

:- setting(sources,
	   atom,
	   '~/src/pl-devel',
	   'Sourced directory for getting changelog').
:- setting(branches,
	   list(any),
	   [ development = 'origin/master',
	     stable = 'stable/master'
	   ],
	   'Branches displayed').
:- setting(default_branch,
	   atom,
	   development,
	   'Default branch to show').

:- http_handler(root('ChangeLog'), changelog, [pool(wiki)]).

%%	changelog(+Request)
%
%	HTTP handler that shows the ChangeLog since a given version.

changelog(Request) :-
	http_parameters(Request,
			[ from(VFrom,    [optional(true)]),
			  to(VTo,        [optional(true)]),
			  branch(Branch, [optional(true)])
			]),
	defaults(VFrom, VTo, Branch),
	changelog(VFrom-VTo, Codes),
	wiki_file_codes_to_dom(Codes, -, DOM),
	(   memberchk(h1(_, TitleParts), DOM)
	->  atomic_list_concat(TitleParts, Title)
	;   Title = 'SWI-Prolog ChangeLog'
	),
	reply_html_page(pldoc(default),
			title(Title),
			[ \alt_branches(Branch), ', ',
			  \alt_versions(Branch, VFrom, VTo)
			| DOM
			]).

defaults(VFrom, VTo, _Branch) :-
	nonvar(VFrom), nonvar(VTo), !.
defaults(VFrom, VTo, Branch) :-
	(   var(Branch)
	->  setting(default_branch, Branch)
	;   true
	),
	branch_versions(Branch, Versions),
	append(_, [VTo,VFrom|_], Versions), !.

changelog(Range, Codes) :-
	setting(sources, SourceDir0),
	expand_file_name(SourceDir0, [SourceDir]),
	directory_file_path(SourceDir, 'scripts/mkchangelog', Script),
	range_arg(Range, Versions),
	setup_call_cleanup(
	    process_create(Script, ['--wiki', Versions],
			   [ stdout(pipe(Out)),
			     cwd(SourceDir)
			   ]),
	    read_stream_to_codes(Out, Codes),
	    close(Out)).

range_arg(From-To, Versions) :-
	atomic_list_concat([From, To], '..', Versions).
range_arg(From, From).

alt_branches(Branch) -->
	{ setting(branches, Branches)
	},
	html([ b('Branch:')
	     | \branches(Branches, Branch)
	     ]).


branches([], _) --> [].
branches([H|T], Def) --> branch(H, Def), branches(T, Def).

branch(B=_, B) --> !,
	html([' ', span(B)]).
branch(B=_, _) -->
	{ http_link_to_id(changelog, [branch(B)], HREF)
	},
	html([' ', a(href(HREF), B)]).

alt_versions(Branch, _VFrom, _VTo) -->
	{ var(Branch) }, !.
alt_versions(Branch, VFrom, VTo) -->
	{ branch_versions(Branch, Versions),
	  http_link_to_id(changelog, [], Action)
	},
	html([ form([action(Action), style('display:inline')],
		    [ input([type(hidden), name(branch), value(Branch)]),
		      b('version'),' ',
		      \select(from, Versions, VFrom),
		      b(' to version '),
		      \select(to, Versions, VTo), ' ',
		      input([ type(submit),
			      value('Update ChangeLog')
			    ])
		    ])
	     ]).

select(Name, Values, Selected) -->
	html(select(name(Name), \values(Values, Selected))).

values([], _) --> [].
values([H|T], Selected) --> value(H, Selected), values(T, Selected).

value(V, V) --> !,
	html(option([selected], V)).
value(V, _) -->
	html(option(V)).

%%	versions(Branch, Versions) is det.
%
%	Retrieve the versions that are available for Branch.

:- dynamic
	version_cache/3.			% Branch, Retrieved, Versions

branch_versions(Alias, Versions) :-
	setting(branches, Map),
	memberchk(Alias=Branch, Map),
	versions(Branch, Versions).

versions(Branch, Versions) :-
	version_cache(Branch, Retrieved, Versions),
	get_time(Now),
	Now-Retrieved < 600, !.
versions(Branch, Versions) :-
	retractall(version_cache(Branch, _, _)),
	versions_no_cache(Branch, Versions),
	get_time(Now),
	assertz(version_cache(Branch, Now, Versions)).

versions_no_cache(Branch, Versions) :-
	git_repo(Repo),
	git_tags_on_branch(Repo, Branch, Tags),
	tags_versions(Tags,  Versions).

tags_versions([], []).
tags_versions([H|T], Versions) :-
	atomic_list_concat(Tags, ', ', H),
	(   Tags = [Tag]
	->  (   atom_concat('V', Version, Tag)
	    ->	Versions = [Version|VT],
		tags_versions(T, VT)
	    ;	tags_versions(T, Versions)
	    )
	;   append(Tags, T, AllTags),
	    tags_versions(AllTags, Versions)
	).

git_repo(Repo) :-
	setting(sources, SourceDir0),
	expand_file_name(SourceDir0, [Repo]).
