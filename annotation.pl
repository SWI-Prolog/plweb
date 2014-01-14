:- module(annotation,
	  [ annotation//1,			% +object:compound
	    user_annotations//1,		% +User:atom
	    user_annotation_count/2		% +User:atom, -Count:nonneg
	  ]).

/** <module> Annotation

@author Wouter Beek
@tbd Build annotation2post converter.
@version 2014/01
*/

:- use_module(generics).
:- use_module(library(dcg/basics)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json_convert)).
:- use_module(library(pldoc/doc_html), [object_ref//2]).
:- use_module(object_support).
:- use_module(openid).
:- use_module(post).

:- html_resource(css('annotation.css'), [requires([css('post.css')])]).

:- multifile(prolog:doc_object_page_footer/2).

http:location(annotation, root(annotation), []).
:- http_handler(root(annotation), annotation_process, [prefix]).

%%	annotation_process(+Request)
%
%	REST HTTP handler for /annotation/ID
%
%	@tbd	Where is this used for?  This also seems to do a request
%		on ourselves.  We'd like to avoid that.

annotation_process(Request):-
	memberchk(method(get), Request),
	request_to_id(Request, annotation, Post), !,
	post(Post, id, Id),
	post(Post, about, Object),
	object_label(Object, Label),
	atomic_list_concat(['Annotation',Label], '--', Title),
	reply_html_page(
	    wiki(Title),
	    title(Title),
	    \post(Id, [])).
annotation_process(Request):-
	post_process(Request, annotation).

%%	annotation(+Object)//
%
%	Show annotations for Object.

annotation(Object) -->
	{ ground(Object), !,
	  (   prolog:doc_canonical_object(Object, Object2)
	  ->  true
	  ;   Object2 = Object
	  ),
	  find_posts(annotation, about_post(Object2), Ids)
	},
	html([\html_requires(css('annotation.css')),
	      \posts(annotation, Object2, Ids)
	     ]).
annotation(_) --> [].

about_post(About, Id) :-
	post(Id, about, About).

%%	user_annotations(+User)//
%
%	Show annotations created by a specific user.

user_annotations(User) -->
	{ find_posts(annotation, user_post(User), Ids),
	  Ids \== [], !,
	  sort_posts(Ids, SortedIds),
	  site_user_property(User, name(Name))
	},
	html([ \html_requires(css('annotation.css')),
	       h2(class(wiki), 'Comments by ~w'-[Name]),
	       table(class('user-comments'),
		     \list_annotated_objects(SortedIds))
	     ]).
user_annotations(_) -->
	[].

user_post(User, Id) :-
	post(Id, author, User).

list_annotated_objects([]) --> [].
list_annotated_objects([H|T]) -->
	{ post(H, about, ObjectId),
	  object_id(Object1, ObjectId),
	  Object1 = comment(Object2,Comment)
	},
	html([ tr([ td(\object_ref(Object2, [])),
		    td(class('comment-summary'),
		       \comment_summary(Comment))
		  ]),
	       \list_annotated_objects(T)
	     ]).

%%	comment_summary(+Comment)//
%
%	Show the first sentence or max first 80 characters of Comment.

comment_summary(Comment) -->
	{ summary_sentence(Comment, Summary) },
	html(Summary).

summary_sentence(Comment, Summary):-
	atom_codes(Comment, Codes),
	phrase(summary(SummaryCodes, 80), Codes, _),
	atom_codes(Summary, SummaryCodes).

summary([C,End], _) -->
	[C,End],
	{ \+ code_type(C, period),
	  code_type(End, period) % ., !, ?
	},
	white, !.
summary([0' |T0], Max) -->
	blank, !,
	blanks,
	{Left is Max-1},
	summary(T0, Left).
summary(Elipsis, 0) --> !,
	{ string_codes(" ...", Elipsis)
	}.
summary([H|T0], Max) -->
	[H], !,
	{Left is Max-1},
	summary(T0, Left).
summary([], _) -->
	[].

%%	user_annotation_count(+User, -Count) is det.
%
%	True when Count is the number of object annotations created by
%	User.

user_annotation_count(User, Count) :-
	find_posts(annotation, user_post(User), Annotations),
	length(Annotations, Count).

