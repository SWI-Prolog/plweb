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


:- module(tagit,
	  [ user_tags//1,		% +User
	    object_label/2,		% +Object, -Label
	    object_id/2			% +Object, -Id
	  ]).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(pldoc/doc_search)).
:- use_module(library(pldoc/doc_html)).
:- use_module(openid).
:- use_module(notify).

:- html_resource(tagit,
		 [ virtual(true),
		   requires([ jquery_ui,
			      js('tagit/css/jquery.tagit.css'),
			      js('tagit/css/tagit.ui-zendesk.css'),
			      js('tagit/js/tag-it.min.js')
			    ])
		 ]).

		 /*******************************
		 *	       DATA		*
		 *******************************/

:- persistent
	tagged(tag:atom,			% Name of the tag
	       object:any,			% Object attached to
	       time:integer,			% When was it tagged
	       user:atom),			% User that added the tag
	tag(tag:atom,
	    time:integer,			% When was it created
	    user:atom).

:- initialization
	db_attach('tags.db',
		  [ sync(close)
		  ]).

current_tag(Tag) :-
	tag(Tag, _, _).

create_tag(Tag, _User) :-
	tag(Tag, _, _), !.
create_tag(Tag, User) :-
	get_time(NowF),
	Now is round(NowF),
	assert_tag(Tag, Now, User), !.


%%	tagit_user(+Request, -User) is det.
%
%	User as seen for tagging. This is either the current user or the
%	peer.

tagit_user(_Request, User) :-
	site_user_logged_in(User), !.
tagit_user(Request, Peer) :-
	http_peer(Request, Peer).


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

:- http_handler(root('complete-tag'), complete_tag, []).
:- http_handler(root('show-tag'),     show_tag,	    []).
:- http_handler(root('add-tag'),      add_tag,	    []).
:- http_handler(root('remove-tag'),   remove_tag,   []).

:- multifile
	prolog:doc_object_page_footer//2,
	prolog:doc_annotation_footer//2.

%%	prolog:doc_object_page_footer(+Object, +Options)//
%
%	Called a to create the footer of an object page.

prolog:doc_object_page_footer(Obj, Options) -->
	{ ground(Obj) }, !,
	html(div(class('user-annotations'),
		 [ \tagit_footer(Obj, Options),
		   \comment_footer(Obj, Options)
		 ])).
prolog:doc_object_page_footer(_, _) -->
	[].

comment_footer(Obj, Options) -->
	prolog:doc_annotation_footer(Obj, Options), !.
comment_footer(_, _) --> [].

%%	tagit_footer(+Obj, +Options)// is det.
%
%	Show tagit widget for adding and deleting tags.

tagit_footer(Obj, _Options) -->
	{ http_link_to_id(complete_tag, [], Complete),
	  http_link_to_id(show_tag, [], OnClick),
	  http_link_to_id(add_tag, [], AddTag),
	  http_link_to_id(remove_tag, [], RemoveTag),
	  object_label(Obj, Label),
	  object_id(Obj, ObjectID),
	  format(atom(PlaceHolder), 'Tag ~w', [Label]),
	  object_tags(Obj, Tags),
	  atomic_list_concat(Tags, ',', Data)
	},
	html([ input([id(tags), value(Data)]),
	       \why_login
	     ]),
	html_requires(tagit),
	js_script(<![javascript(Complete, OnClick, PlaceHolder, ObjectID,
				AddTag, RemoveTag)
		     [$(document).ready(function() {
		        $("#tags").tagit({
			    autocomplete: { delay: 0.3,
					    minLength: 1,
					    source: Complete
					  },
			    onTagClicked: function(event, ui) {
			      window.location.href = OnClick+"?tag="+
				encodeURIComponent(ui.tagLabel);
			    },
			    beforeTagAdded: function(event, ui) {
			      if ( !ui.duringInitialization ) {
				$.ajax({ dataType: "json",
					 url: AddTag,
					 data: { tag: ui.tagLabel,
						 obj: ObjectID
					       }
				       });
			      }
			    },
			    beforeTagRemoved: function(event, ui) {
			      $.ajax({ dataType: "json",
				       url: RemoveTag,
				       data: { tag: ui.tagLabel,
					       obj: ObjectID
					     }
				     });
			    },
			    removeConfirmation: true,
			    placeholderText: PlaceHolder,
			    singleField: true
			  });
		      });
		     ]]>).

why_login -->
	{ site_user_logged_in(_) }, !.
why_login -->
	html(div(class('login-keeps-profile'),
		 'Tags are associated to your profile if you are logged in')).

%%	object_label(+Object, -Label) is det.

object_label(Name/Arity, Label) :- !,
	format(atom(Label), 'predicate ~w/~w', [Name, Arity]).
object_label(Name//Arity, Label) :- !,
	format(atom(Label), 'non-terminal ~w/~w', [Name, Arity]).
object_label(M:Name/Arity, Label) :- !,
	format(atom(Label), 'predicate ~w:~w/~w', [M, Name, Arity]).
object_label(M:Name//Arity, Label) :- !,
	format(atom(Label), 'non-terminal ~w:~w//~w', [M, Name, Arity]).
object_label(f(Name/Arity), Label) :- !,
	format(atom(Label), 'function ~w/~w', [Name, Arity]).
object_label(Module:module(_Title), Label) :-
	module_property(Module, file(File)), !,
	file_base_name(File, Base),
	format(atom(Label), 'module ~w', [Base]).
object_label(section(Level, Number, F), Label) :-
	prolog:doc_object_summary(section(Level, Number, SF),
				  _Class, _AbsFile, Title),
	doc_same_file(F, SF), !,
	format(atom(Label), 'Section ~w: ~w', [Number, Title]).
object_label(Obj, Label) :-
	term_to_atom(Obj, Label).

doc_same_file(F, F) :- !.
doc_same_file(swi(F), Abs) :-
	sub_atom(Abs, _, _, 0, F).

object_tags(Object, Tags) :-
	findall(Tag, tagged(Tag, Object, _Time, _User), Tags0),
	sort(Tags0, Tags).

%%	object_id(?Object, ?Id)
%
%	Manage identifiers for objects.

:- dynamic
	object_id_cache/2.

object_id(Object, Id) :-
	object_id_cache(Object, Id), !.
object_id(Object, Id) :-
	ground(Object),
	variant_sha1(Object, Id),
	assertz(object_id_cache(Object, Id)).


%%	complete_tag(+Request)
%
%	Complete.  Currently only uses existing tags for completion.
%
%	@tbd	Provide pre-populated completion (e.g., from FOLDOC)
%	@tbd	Show (as feedback) how often this is used, etc.

complete_tag(Request) :-
	http_parameters(Request,
			[ term(Q, [])
			]),
	debug(tag(autocomplete), 'Autocomplete ~q', [Q]),
	(   setof(A, tag_holding(Q,A), List)
	->  true
	;   List = []
	),
	reply_json(List).

tag_holding(Term, Tag) :-
	current_tag(Tag),
	(   sub_atom(Tag, _, _, _, Term)
	->  true
	).

%%	add_tag(+Request)
%
%	Add tag to the given object

add_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, []),
			  obj(Hash, [])
			]),
	object_id(Object, Hash),
	tagit_user(Request, User),
	debug(tagit, 'add_tag: ~q: ~q to ~q', [User, Tag, Object]),
	create_tag(Tag, User),
	get_time(NowF),
	Now is round(NowF),
	assert_tagged(Tag, Object, Now, User),
	notify(Object, tagged(Tag)),
	reply_json(true).

%%	remove_tag(+Request)
%
%	Remove tag from the given object

remove_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, []),
			  obj(Hash, [])
			]),
	object_id(Object, Hash),
	tagit_user(Request, User),
	debug(tagit, 'remove_tag: ~q: ~q to ~q', [User, Tag, Object]),
	(   retract_tagged(Tag, Object, _, User)
	->  notify(Object, untagged(Tag)),
	    reply_json(true)
	;   reply_json(false)
	).


%%	show_tag(+Request)
%
%	Show pages that are tagged with this tag.

show_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, [])
			]),
	findall(Obj, tagged(Tag, Obj, _, _), Objects0),
	sort(Objects0, Objects),
	reply_html_page(wiki(tags),
			title('Pages tagged "~w"'-[Tag]),
			[ h1(class(wiki), 'Pages tagged "~w"'-[Tag]),
			  \doc_resources([]),
			  \matching_object_table(Objects, [])
			]).


		 /*******************************
		 *   AUTOCOMPLETE INTEGRATION	*
		 *******************************/

:- multifile
	prolog:ac_object/3,
	prolog:doc_object_href/2,		% +Object, -HREF
	prolog:doc_object_label_class/3,
	prolog:ac_object_attributes/2.

%%	prolog:ac_object(+MatchHow, +Term, -Match) is nondet.
%
%	Provide additional autocompletion matches on tags,
%

prolog:ac_object(name, Term, Tag-tag(Tag)) :-
	current_tag(Tag),
	(   sub_atom(Tag, 0, _, _, Term)
	->  true
	).
prolog:ac_object(token, Term, Tag-tag(Tag)) :-
	current_tag(Tag),
	(   sub_atom(Tag, _, _, _, Term)
	->  true
	).

prolog:doc_object_href(tag(Tag), HREF) :-
	http_link_to_id(show_tag, [tag(Tag)], HREF).

prolog:doc_object_label_class(tag(Tag), Tag, tag).

prolog:ac_object_attributes(tag(Tag), [tag=Info]) :-
	aggregate_all(count, tagged(Tag,_,_,_), Used),
	format(atom(Info), 'tag x~D', [Used]).


		 /*******************************
		 *	      PROFILE		*
		 *******************************/

%%	user_tags(+User)
%
%	Show all tags created by a given user.

user_tags(User) -->
	{ findall(Tag-Obj, tagged(Tag, Obj, _Time, User), Pairs),
	  Pairs \== [], !,
	  keysort(Pairs, Sorted),
	  group_pairs_by_key(Sorted, Keyed),
	  site_user_property(User, name(Name))
	},
	html([ h2(class(wiki), 'Tags by ~w'-[Name]),
	       ul(class('user-tags'),
		  \list_tags(Keyed))
	     ]).
user_tags(_) --> [].

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


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	mail_notify:event_subject//1,		% +Event
	mail_notify:event_message//1.		% +event

mail_notify:event_subject(tagged(Tag)) -->
	[ 'tagged with ~w'-[Tag] ].
mail_notify:event_subject(untagged(Tag)) -->
	[ 'removed tag ~w'-[Tag] ].


mail_notify:event_message(tagged(Tag)) -->
	[ 'tagged with ~w'-[Tag] ].
mail_notify:event_message(untagged(Tag)) -->
	[ 'removed tag ~w'-[Tag] ].
