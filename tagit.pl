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
	  [ tagit//2				% +Tags, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(persistency)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(pldoc/doc_search)).
:- use_module(library(pldoc/doc_html)).
:- use_module(openid).

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
	prolog:doc_object_page_footer//2.

%%	prolog:doc_object_page_footer(+Object, +Options)//
%
%	Called a to create the footer of an object page.

prolog:doc_object_page_footer(Obj, _Options) -->
	{ http_link_to_id(complete_tag, [], Complete),
	  http_link_to_id(show_tag, [], ShowTag),
	  http_link_to_id(add_tag, [], AddTag),
	  http_link_to_id(remove_tag, [], RemoveTag),
	  object_label(Obj, Label),
	  object_id(Obj, Id),
	  format(atom(PlaceHolder), 'Tag ~w', [Label]),
	  object_tags(Obj, Tags)
	},
	html(div(class('user-annotations'),
		 [ \tagit(Tags,
			  [ autocomplete(Complete),
			    remove_confirmation(true),
			    on_click(ShowTag),
			    before_tag_added(AddTag),
			    before_tag_removed(RemoveTag),
			    placeholder(PlaceHolder),
			    object_id(Id)
			  ])
		 ])).

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
object_label(Obj, Label) :-
	term_to_atom(Obj, Label).

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
	->  reply_json(true)
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
	prolog:doc_object_label_class/3.

%%	prolog:ac_object(+MatchHow, +Term, -Match) is nondet.
%
%	Provide additional autocompletion matches on tags,
%

prolog:ac_object(_How, Term, Tag-tag(Tag)) :-
	current_tag(Tag),
	(   sub_atom(Tag, _, _, _, Term)
	->  true
	).

prolog:doc_object_href(tag(Tag), HREF) :-
	http_link_to_id(show_tag, [tag(Tag)], HREF).

prolog:doc_object_label_class(tag(Tag), Label, tag) :-
	aggregate_all(count, tagged(Tag,_,_,_), Used),
	format(atom(Label), '~w (tag x ~D)', [Tag, Used]).


		 /*******************************
		 *	     TAGIT UI		*
		 *******************************/

%%	tagit(+Tags, +Options)// is det.
%
%	HTML rule to include a tagit object

tagit(Tags, Options) -->
	{ option(id(Id), Options, tags),
	  atomic_list_concat(Tags, ',', Data)
	},
	html_requires(tagit),
	html([ input([id(Id), value(Data)]),
	       script([type('text/javascript')],
		      [ \[ '$(document).ready(function() {\n',
			   '  $("#',Id,'").tagit({\n'
			 ],
		      \tagit_options(Options, Options),
			\[ '    singleField:true\n',
			   '  });\n',
			   '});\n'
			 ]
		      ])
	     ]).

tagit_options([], _) --> [].
tagit_options([H|T], Options) -->
	(   tagit_option(H, Options)
	->  html(\[',\n'])
	;   []
	),
	tagit_options(T, Options).

tagit_option(before_tag_added(URL), Options) --> !,
	{ option(object_id(Id), Options, -) },
	html(\['    beforeTagAdded: function(event, ui) {\n',
	       '      if ( !ui.duringInitialization ) {\n',
	       '        $.ajax({\n',
	       '          dataType: "json",\n',
	       '	  url:"~w",\n'-[URL],
	       '          data: {\n',
	       '                  tag:ui.tagLabel,\n',
	       '                  obj:"~w"\n'-[Id],
	       '                }\n',
	       '        });\n',
	       '      }\n',
	       '    }'
	      ]).
tagit_option(before_tag_removed(URL), Options) --> !,
	{ option(object_id(Id), Options, -) },
	html(\['    beforeTagRemoved: function(event, ui) {\n',
	       '      $.ajax({\n',
	       '        dataType: "json",\n',
	       '	url:"~w",\n'-[URL],
	       '        data: {\n',
	       '                tag:ui.tagLabel,\n',
	       '                obj:"~w"\n'-[Id],
	       '              }\n',
	       '      });\n',
	       '    }'
	      ]).
tagit_option(Option, _) -->
	tagit_option(Option).

tagit_option(autocomplete(URL)) -->
	html(\['    autocomplete: {\n',
	       '      delay: 0.3,\n',
	       '      minLength: 1,\n',
	       '      source:"~w"\n'-[URL],
	       '    }'
	      ]).
tagit_option(remove_confirmation(true)) -->
	html(\['    removeConfirmation: true']).
tagit_option(on_click(URL)) -->
	html(\['    onTagClicked: function(event, ui) {\n',
	       '      window.location.href = "~w?tag="+\c
		      encodeURIComponent(ui.tagLabel);\n'-[URL],
	       '    }'
	      ]).
tagit_option(placeholder(Text)) -->
	html(\['    placeholderText: "~w"'-[Text]]).
