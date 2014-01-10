:- module(
  annotation,
  [
    annotation//1, % +Object:compound
    user_annotations//1, % +User:atom
    user_annotation_count/2	% +User:atom
                            % -Count:nonneg
  ]
).

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


annotation_process(Request):-
  memberchk(method(get), Request),
  request_to_resource(Request, annotation, URL), !,
  http_get(URL, JSON, []),
  json_to_prolog(JSON, post:Post),
  post(Post, id, Id),
  post(Post, about, About),
  object_id(Object, About),
  object_label(Object, Label),
  atomic_list_concat(['Annotation',Label], '--', Title),
  reply_html_page(wiki(Title), title(Title), \post([], About, Id)).
annotation_process(Request):-
  post_process(Request, annotation).

annotation(Object1) -->
  {
    ground(Object1),
    (
      prolog:doc_canonical_object(Object1, Object2)
    ->
      true
    ;
      Object2 = Object1
    ),
    object_id(Object2, About), !,
    find_posts(annotation, '_check1'(About), Ids)
  },
  html([
    \html_requires(css('annotation.css')),
    \posts(annotation, About, Ids)
  ]).
'_check1'(About, Id):- post(Id, about, About).

user_annotations(User) -->
  {
    find_posts(annotation, '_check2'(User), Ids),
    Ids \== [], !,
    sort_posts(Ids, SortedIds),
    site_user_property(User, name(Name))
  },
  html([
    \html_requires(css('annotation.css')),
    h2(class(wiki), 'Comments by ~w'-[Name]),
    table(class('user-comments'), \list_annotated_objects(SortedIds))
  ]).
user_annotations(_) --> [].
'_check2'(User, Id):- post(Id, author, User).

list_annotated_objects([]) --> [].
list_annotated_objects([H|T]) -->
  {
    post(H, about, ObjectId),
    object_id(Object1, ObjectId),
    Object1 = comment(Object2,Comment)
  },
  html([
    tr([
      td(\object_ref(Object2, [])),
      td(class('comment-summary'), \comment_summary(Comment))
    ]),
    \list_annotated_objects(T)
  ]).

comment_summary(Comment) -->
  {summary_sentence(Comment, Summary)},
  html(Summary).

summary_sentence(Comment, Summary):-
  atom_codes(Comment, Codes),
  phrase(summary(SummaryCodes, 80), Codes, _),
  atom_codes(Summary, SummaryCodes).

summary([C,End], _) -->
  [C,End],
  {
    \+ code_type(C, period),
    code_type(End, period) % ., !, ?
  },
  white, !.
summary([0' |T0], Max) -->
  blank, !,
  blanks,
  {Left is Max-1},
  summary(T0, Left).
summary(Elipsis, 0) --> !,
  {string_codes(" ...", Elipsis)}.
summary([H|T0], Max) -->
  [H], !,
  {Left is Max-1},
  summary(T0, Left).
summary([], _) --> [].

user_annotation_count(User, Count) :-
  find_posts(annotation, '_check2'(User), Annotations),
  length(Annotations, Count).

