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

:- http_handler(root('ChangeLog'), changelog, [pool(wiki)]).

%%	changelog(+Request)
%
%	HTTP handler that shows the ChangeLog since a given version.

changelog(Request) :-
	http_parameters(Request,
			[ from(VFrom, [optional(true)]),
			  to(VTo, [optional(true)])
			]),
	(   var(VFrom)
	->  reply_html_page(pldoc(default),
			    title('Get ChangeLog since version'),
			    \changelog_form)
	;   (   var(VTo)
	    ->	Range = VFrom
	    ;	Range = (VFrom-VTo)
	    ),
	    changelog(Range, Codes),
	    wiki_file_codes_to_dom(Codes, -, DOM),
	    (	memberchk(h1(_, TitleParts), DOM)
	    ->	atomic_list_concat(TitleParts, Title)
	    ;	Title = 'SWI-Prolog ChangeLog'
	    ),
	    reply_html_page(pldoc(default),
			    title(Title),
			    DOM)
	).


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

changelog_form -->
	html(tbd).
