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
	  [ tagit//1
	  ]).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

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

		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

:- http_handler(root('complete-tag'), complete_tag, []).
:- http_handler(root('show-tag'),     show_tag,	    []).

:- multifile
	prolog:doc_object_page_footer//2.

%%	prolog:doc_object_page_footer(+Object, +Options)//
%
%	Called a to create the footer of an object page.

prolog:doc_object_page_footer(Obj, _Options) -->
	{ http_link_to_id(complete_tag, [], Complete),
	  http_link_to_id(show_tag, [], ShowTag),
	  object_label(Obj, Label),
	  format(atom(PlaceHolder), 'Tag ~w', [Label])
	},
	html(div(class('user-annotations'),
		 [ \tagit([ autocomplete(Complete),
			    remove_confirmation(true),
			    on_click(ShowTag),
			    placeholder(PlaceHolder)
			  ])
		 ])).

object_label(_M:Name/Arity, Label) :- !,
	format(atom(Label), '~w/~w', [Name, Arity]).
object_label(_M:Name//Arity, Label) :- !,
	format(atom(Label), '~w//~w', [Name, Arity]).
object_label(f(Name/Arity), Label) :- !,
	format(atom(Label), '~w/~w', [Name, Arity]).
object_label(Module:module(_Title), Label) :-
	module_property(Module, file(File)), !,
	file_base_name(File, Base),
	format(atom(Label), 'module ~w', [Base]).
object_label(Obj, Label) :-
	term_to_atom(Obj, Label).


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

%%	show_tag(+Request)
%
%	Show pages that are tagged with this tag.

show_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, [])
			]),
	reply_html_page(pldoc,
			title('Pages tagged "~w"'-[Tag]),
			[ h1('Pages tagged "~w"'-[Tag])
			]).


		 /*******************************
		 *	     TAGIT UI		*
		 *******************************/

%%	tagit(+Options)// is det.
%
%	HTML rule to include a tagit object

tagit(Options) -->
	{ option(id(Id), Options, tags)
	},
	html_requires(tagit),
	html([ input([id(Id)]),
	       script([type('text/javascript')],
		      [ \[ '$(document).ready(function() {\n',
			   '  $("#',Id,'").tagit({\n'
			 ],
		      \tagit_options(Options),
			\[ '    dummy:true\n',
			   '  });\n',
			   '});\n'
			 ]
		      ])
	     ]).

tagit_options([]) --> [].
tagit_options([H|T]) -->
	(   tagit_option(H)
	->  html(\[',\n'])
	;   []
	),
	tagit_options(T).

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
