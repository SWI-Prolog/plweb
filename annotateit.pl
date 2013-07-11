/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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


:- module(annotateit,
	  [ user_annotations//1,	% +User
	    user_annotation_count/2	% +User, -Count
	  ]).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(aggregate)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/doc_html), [object_ref/4, object_href/2]).
:- use_module(wiki).
:- use_module(openid).
:- use_module(tagit).
:- use_module(markitup).
:- use_module(notify).

/** <module> Allow (logged on) users to comment on manual objects
*/


		 /*******************************
		 *	       DATA		*
		 *******************************/

:- persistent
	annotation(object:any,			% Object attached to
		   annotation:atom,		% Text of the annotation
		   time:integer,		% When was it tagged
		   user:atom).			% User that added the tag


:- initialization
	db_attach('annotations.db',
		  [ sync(close)
		  ]).

user_annotation_count(User, Count) :-
	aggregate_all(count, annotation(_,_,_,User), Count).


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

:- http_handler(root('add-annotation'), add_annotation, []).

:- multifile
	prolog:doc_annotation_footer//2.

%%	prolog:doc_annotation_footer(+Object, +Options)//
%
%	Called a to create the footer of an object page.

prolog:doc_annotation_footer(Obj, Options) -->
	html(div(class(comments),
		 [ \object_annotations(Obj, Options),
		   \add_annotation(Obj, Options)
		 ])).

%%	object_annotations(+Object, +Options)// is det.
%
%	Display existing object annotations.

object_annotations(Obj, Options) -->
	{ findall(annotation(Obj, Annot, Time, User),
		  annotation(Obj, Annot, Time, User),
		  Annotations)
	},
	(   {Annotations == []}
	->  []
	;   show_annotations(Annotations, Options)
	).

show_annotations([], _) --> [].
show_annotations([H|T], Options) -->
	show_annotation(H, Options),
	show_annotations(T, Options).

show_annotation(annotation(_Obj, Annot, Time, User), _Options) -->
	html(div(class(annotation),
		 [ div(class(comment),
		       \comment(Annot)),
		   div(class('commenter'),
		       [ \date(Time), ', ',
			 \user_profile_link(User)
		       ])
		 ])).

comment(Text) -->
	{ atom_codes(Text, Codes),
	  wiki_file_codes_to_dom(Codes, /, DOM0),
	  clean_dom(DOM0, DOM)
	},
	html(DOM).

clean_dom([p(X)], X) :- !.
clean_dom(X, X).


date(Time) -->
	{ format_time(atom(Date), '%A %d %B %Y', Time)
	},
	html(span(class(date), Date)).

%%	add_annotation(+Object, +Options)// is det.
%
%	Allow adding a new annotation if the user is logged on.

add_annotation(Obj, _Options) -->
	{ site_user_logged_in(User), !,
	  http_link_to_id(add_annotation, [], AddAnnotation),
	  object_label(Obj, Label),
	  object_id(Obj, ObjectID),
	  (   annotation(Obj, Current, _Time, User)
	  ->  Extra = [value(Current)]
	  ;   Extra = []
	  )
	},
	html([ div(class('comment-action'),
		   [ \add_open_link(Current, comment_form),
		     ' comment for ~w'-[Label]
		   ]),
	       form([ action(AddAnnotation),
		      id(comment_form),
		      method('POST'),
		      style('display:none')
		    ],
		    [ input([type(hidden), name(object), value(ObjectID)]),
		      \explain_comment(User),
		      table([ tr(td(\markitup([ id(comment),
						markup(pldoc)
					      | Extra
					      ]))),
			      tr(td(align(right),
				    input([ type(submit),
					    value('Save comment')
					  ])))
			    ])
		    ])
	     ]).
add_annotation(_Obj, _Options) -->
	{ http_current_request(Request)
	},
	html(div(class('comment-login'),
		 [ \login_link(Request),
		   ' to add a comment'
		 ])).

%%	add_open_link(+Current, +Id)// is det.
%
%	Add a link to actually open the editor.

add_open_link(Current, Id) -->
	{ (   var(Current)
	  ->  Label = 'Add'
	  ;   Label = 'Edit or remove your'
	  )
	},
	html(a([ class('edit-comment'),
		 href(''),
		 onClick('$("#'+Id+'").css("display","block"); return false')
	       ],
	       Label)).

explain_comment(_User) -->
	html({|html||
	      <div class="explain-comment">
	      Comments are intended to point to <b>related material</b>,
	      indicate <b>errors</b> or provide <b>examples</b>.<br>
	      Comments are written using the PlDoc wiki format.  Preview
	      may be enabled and disabled with the two rightmost buttons.
	      </div>
	      |}).


%%	add_annotation(+Request)
%
%	HTTP handler to add a new annotation to a manual object.  The
%	page is reloaded if this is successful.

add_annotation(Request) :-
	http_parameters(Request,
			[ object(ObjectID, []),
			  comment(Annotation0, [default('')])
			]),
	site_user_logged_in(User),
	object_id(Object, ObjectID),
	get_time(NowF),
	Now is round(NowF),
	(   annotation(Object, Old, OldTime, User)
	->  true
	;   Old = ''
	),
	(   normalize_space(atom(''), Annotation0)
	->  Annotation = ''
	;   Annotation = Annotation0
	),
	(   Old == Annotation
	->  true
	;   ignore(retract_annotation(Object, _, _, User)),
	    (   Annotation == ''
	    ->  true
	    ;   assert_annotation(Object, Annotation, Now, User)
	    ),
	    notify_updated(Old, Annotation, OldTime, Now, Object, User)
	),
	object_href(Object, HREF),
	http_redirect(moved_temporary, HREF, Request).

notify_updated('', New, _, Time, Object, User) :- !,
	notify(Object, annotation_added(User, Time, New)).
notify_updated(Old, '', OldTime, Time, Object, User) :- !,
	notify(Object, annotation_removed(User, OldTime, Time, Old)).
notify_updated(Old, New, OldTime, Time, Object, User) :-
	notify(Object, annotation_updated(User, OldTime, Time, Old, New)).


		 /*******************************
		 *	      PROFILE		*
		 *******************************/

%%	user_annotations(+User)// is det.
%
%	Show object that are annotated by user.

user_annotations(User) -->
	{ findall(Time-comment(Obj,Comment),
		  annotation(Obj, Comment, Time, User),
		  Pairs),
	  Pairs \== [], !,
	  sort(Pairs, Sorted),
	  pairs_values(Sorted, Objects),
	  site_user_property(User, name(Name))
	},
	html([ h2(class(wiki), 'Comments by ~w'-[Name]),
	       table(class('user-comments'),
		     \list_annotated_objects(Objects))
	     ]).
user_annotations(_) --> [].

list_annotated_objects([]) --> [].
list_annotated_objects([H|T]) -->
	list_annotated_object(H),
	list_annotated_objects(T).

list_annotated_object(comment(Obj, Comment)) -->
	html(tr([ td(\object_ref(Obj, [])),
		  td(class('comment-summary'), \comment_summary(Comment))
		])).

comment_summary(Comment) -->
	{ summary_sentence(Comment, Summary)
	},
	html(Summary).

summary_sentence(Comment, Summary) :-
	atom_codes(Comment, Codes),
	phrase(summary(SummaryCodes, 80), Codes, _),
	atom_codes(Summary, SummaryCodes).


summary([C,End], _) -->
	[C,End],
	{ \+ code_type(C, period),
	  code_type(End, period)		% ., !, ?
	},
	white, !.
summary([0' |T0], Max) -->
	blank, !,
	blanks,
	{ Left is Max-1 },
	summary(T0, Left).
summary(" ...", 0) --> !.
summary([H|T0], Max) -->
	[H], !,
	{ Left is Max-1 },
	summary(T0, Left).
summary([], _) --> [].


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	mail_notify:event_subject//1,		% +Event
	mail_notify:event_message//1.		% +event

mail_notify:event_subject(annotation_added(User, _, _)) -->
	[ 'Comment by '-[] ],
	msg_user(User).
mail_notify:event_subject(annotation_removed(User, _, _, _)) -->
	[ 'Comment removed by '-[] ],
	msg_user(User).
mail_notify:event_subject(annotation_updated(User, _, _, _, _)) -->
	[ 'Comment updated by '-[] ],
	msg_user(User).

mail_notify:event_message(annotation_added(User, _, New)) -->
	[ 'Comment by '-[] ],
	msg_user(User), [nl],
	msg_body(New).
mail_notify:event_message(annotation_removed(User, _OldT, _T, Old)) -->
	[ 'Comment removed by '-[] ],
	msg_user(User), [nl],
	msg_body(Old).
mail_notify:event_message(annotation_updated(User, _OldT, _T, _Old, New)) -->
	[ 'Comment updated by '-[] ],
	msg_user(User), [nl],
	msg_body(New).

msg_body(Body) -->
	[ nl,
	  '~w'-[Body],
	  nl
	].
