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
	  [ user_annotations//1			% +User
	  ]).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/doc_html), [object_ref/4]).
:- use_module(wiki).
:- use_module(openid).
:- use_module(tagit).
:- use_module(markitup).

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
	object_annotations(Obj, Options),
	add_annotation(Obj, Options).

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
			 \user(User)
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


user(UUID) -->
	{ site_user_property(UUID, name(Name)),
	  http_link_to_id(view_profile, [user(UUID)], HREF)
	}, !,
	html(a([class(user), href(HREF)], Name)).

date(Time) -->
	{ format_time(atom(Date), '%A %d %B %Y', Time)
	},
	html(span(class(date), Date)).

%%	add_annotation(+Object, +Options)// is det.
%
%	Allow adding a new annotation if the user is logged on.

add_annotation(Obj, _Options) -->
	{ site_user_logged_in(_User), !,
	  http_link_to_id(add_annotation, [], AddAnnotation),
	  object_label(Obj, Label),
	  object_id(Obj, ObjectID)
	},
	html(form([action(AddAnnotation), method('POST')],
		  [ div('Add comment for ~w'-[Label]),
		    input([type(hidden), name(object), value(ObjectID)]),
		    \markitup([ id(comment),
				rows(5)
			      ]),
		    input([type(submit)])
		  ])).
add_annotation(_Obj, _Options) -->
	{ http_current_request(Request)
	},
	html(div(class('comment-login'),
		 [ \login_link(Request),
		   ' to add a comment'
		 ])).


%%	add_annotation(+Request)
%
%	HTTP handler to add a new annotation to a manual object.  The
%	page is reloaded if this is successful.

add_annotation(Request) :-
	http_parameters(Request,
			[ object(ObjectID, []),
			  comment(Annotation, [default('')])
			]),
	site_user_logged_in(User),
	object_id(Object, ObjectID),
	get_time(NowF),
	Now is round(NowF),
	assert_annotation(Object, Annotation, Now, User),
	object_href(Object, HREF),
	http_redirect(moved_temporary, HREF, Request).


		 /*******************************
		 *	      PROFILE		*
		 *******************************/

%%	user_annotations(+User)
%
%	Show object that are annotated by user.

user_annotations(User) -->
	{ findall(Obj-Annot, annotation(Obj, Annot, _Time, User), Pairs),
	  Pairs \== [],
	  keysort(Pairs, Sorted),
	  group_pairs_by_key(Sorted, Keyed),
	  site_user_property(User, name(Name))
	},
	html([ h2(class(wiki), 'Tags by ~w'-[Name]),
	       ul(class('user-tags'),
		  \list_tags(Keyed))
	     ]).

list_tags([]) --> [].
list_tags([H|T]) --> list_tag(H), list_tags(T).

list_tag(Tag-Objects) -->
	{ http_link_to_id(show_tag, [tag(Tag)], HREF)
	},
	html(li([ a([class(tag),href(HREF)], Tag),
		  \objects(Objects)
		])).

objects([]) --> [].
objects([H|T]) -->
	object_ref(H, []),
	(   { T == [] }
	->  []
	;   html(', '),
	    objects(T)
	).
