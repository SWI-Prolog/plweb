/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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

:- module(git_html,
	  [ git_shortlog//2,		% +Dir, +Options
	    git_commit_info//3		% +Dir, +Hash, +Options
	  ]).
:- use_module(git).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/dcg_basics)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).

:- html_meta
	odd_even_row(+, -, html, ?, ?).

:- predicate_options(git_commit_info//3, 3,
		     [ diff(oneof([patch,stat])),
		       pass_to(git:git_show/4, 4)
		     ]).
:- predicate_options(git_shortlog//2, 2,
		     [ pass_to(git:git_shortlog/3, 3)
		     ]).

:- http_handler(root(git_show), git_show, []).

%%	git_shortlog(+Dir, +Options)//
%
%	Component that show the top-N most recent changes in Pack.

git_shortlog(Dir, Options) -->
	{ git_shortlog(Dir, ShortLog, Options) },
	html([ h3('Recent changes'),
	       table(class(git_shortlog),
		     \shortlog_rows(ShortLog, Dir, 1))
	     ]).

shortlog_rows([], _, _) --> [].
shortlog_rows([H|T], Pack, Row) -->
	odd_even_row(Row, Next, \shortlog_row(H, Pack)),
	shortlog_rows(T, Pack, Next).

shortlog_row(Record, Pack) -->
	html([ \td_git_log(Pack, author_date_relative, Record),
	       \td_git_log(Pack, author_name, Record),
	       \td_git_log(Pack, subject_and_refnames, Record)
	     ]).

td_git_log(Pack, subject_and_refnames, Record) --> !,
	{ git_log_data(subject, Record, Subject),
	  git_log_data(ref_names, Record, RefNames),
	  git_log_data(commit_hash, Record, Commit),
	  http_link_to_id(git_show, [a(commit),h(Commit),r(Pack)], HREF)
	},
	html(td(class(subject),
		[ a(href(HREF), \trunc(Subject, 50)), \ref_names(RefNames)])).
td_git_log(_, Field, Record) -->
	{ git_log_data(Field, Record, Value),
	  (   Value == ''
	  ->  Class = empty
	  ;   Class = Field
	  )
	},
	html(td(class(Class), Value)).

ref_names([]) --> !.
ref_names(List) -->
	html(span(class(ref_names), \ref_name_list(List))).

ref_name_list([]) --> [].
ref_name_list([H|T]) -->
	html(span(class(ref_name), H)), ref_name_list(T).

trunc(Text, Max) -->
	{ truncate_atom(Text, Max, Show) },
	html(Show).


%%	git_show(+Request) is det.
%
%	HTTP handler to handle GIT requests.

git_show(Request) :-
	http_parameters(Request,
			[ a(_Action,
			    [ oneof([commit]),
			      description('Action to perform')
			    ]),
			  h(Hash,
			    [ description('Hash to work on')
			    ]),
			  r(Dir,
			    [ description('Git directory')
			    ]),
			  diff(Diff,
			       [ oneof([stat,patch]),
				 default(stat),
				 description('Diff-style for commit')
			       ])
			]),
	file_base_name(Dir, Base),
	reply_html_page(cliopatria(cpack),
			title('Commit info'),
			[ h1(class(wiki), [Base, /, commit]),
			  \html_requires(css('plweb.css')),
			  \git_commit_info(Dir, Hash, [diff(Diff)])
			]).


%%	git_commit_info(+Dir, +Hash, +Options)//
%
%	Component to show an individual commit.  Options:
%
%	  * diff(Diff)
%	  One of =stat= (default) or =patch= (full difference)

git_commit_info(Dir, Hash, Options) -->
	{ select_option(diff(Diff), Options, Rest, stat),
	  git_show(Dir, Hash, Record-Body, [diff(Diff)|Rest]),
	  git_commit_data(subject, Record, Subject)
	},
	html_requires(css('git.css')),
	html(div(class(cpack),
		 [ h2(class(wiki), Subject),
		   table(class(commit),
			 [ \tr_commit(author,	 author_name, Record),
			   \tr_commit('',        author_date, Record),
			   \tr_commit(committer, committer_name, Record),
			   \tr_commit('',        committer_date, Record),
			   tr([th(commit),       td(class(commit), Hash)]),
			   \tr_commit(tree,      tree_hash, Record),
			   \tr_commit(parent,    parent_hashes, Record)
			 ]),
		   \select_diff(Diff),
		   pre(class(commitdiff),
		       \diff_lines(Body, Diff))
		 ])).

select_diff(Now) -->
	{ other_diff(Now, Other),
	  http_current_request(Request),
	  http_reload_with_parameters(Request, [diff(Other)], HREF)
	},
	html(div(class(diffstyle),
	       ['Diff style: ', b(Now), ' ', a(href(HREF), Other)])).

other_diff(patch, stat).
other_diff(stat, patch).

diff_lines([], _) --> [].
diff_lines([Line|T], Diff) -->
	(   { diff_line_class(Line, Diff, Class) }
	->  html(span(class(Class), ['~s'-[Line]]))
	;   diff_line(Line, Diff)
	->  []
	;   html('~s'-[Line])
	),
	(   {T==[]}
	->  []
	;   ['\n'],
	    diff_lines(T, Diff)
	).

term_expansion(diff_line_class(Start, Diff, Class),
	       diff_line_class(Codes, Diff, Class)) :-
	is_list(Start),
	append(Start, _, Codes).

diff_line_class("diff ", patch, diff).
diff_line_class("--- ", patch, a).
diff_line_class("+++ ", patch, b).
diff_line_class("-", patch, del).
diff_line_class("+", patch, add).

diff_line(Line, stat) -->
	{ phrase(dirstat(File, Sep, Count, Plusses, Minus), Line) },
	html([ ' ', span(class(file), '~s'-[File]),
	       '~s'-[Sep],
	       '~s'-[Count], ' ',
	       span(class(add), '~s'-[Plusses]),
	       span(class(del), '~s'-[Minus])
	     ]).

dirstat(File, Sep, [D0|RD], Plusses, Minus) -->
	" ",
	string_without(" ", File),
	string(Sep),
	digit(D0),digits(RD),
	" ",
	codes("+", Plusses),
	codes("-", Minus).

codes(Set, [H|T]) --> [H], { memberchk(H, Set) }, !, codes(Set, T).
codes(_, []) --> [].


tr_commit(Label, Field, Record) -->
	{ git_commit_data(Field, Record, Value) },
	html(tr([th(Label), td(class(Field), Value)])).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	odd_even_row(+Row, -Next, :Content)//
%
%	Create odd/even alternating table rows from a DCG.

odd_even_row(Row, Next, Content) -->
	{ (   Row mod 2 =:= 0
	  ->  Class = even
	  ;   Class = odd
	  ),
	  Next is Row+1
	},
	html(tr(class(Class), Content)).

%%	truncate_atom(+Atom, +MaxLen, -Truncated) is det.
%
%	If Atom is longer than MaxLen, truncate  it. If MaxLen is =inf=,
%	Truncated is unified with Atom.

truncate_atom(Atom, inf, All) :- !,
	All = Atom.
truncate_atom(Atom, MaxLen, Truncated) :-
	atom_length(Atom, Len),
	(   Len =< MaxLen
	->  Truncated = Atom
	;   TLen is max(3, MaxLen-4),
	    sub_atom(Atom, 0, TLen, _, S0),
	    atom_concat(S0, ' ...', Truncated)
	).
