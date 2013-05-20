/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2013, VU University Amsterdam

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

:- module(plweb_autocomplete,
	  [
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(apply)).

:- multifile
	prolog:doc_search_field//1,
	prolog:ac_object/3,			% +How, +Search, -Name-Object
	prolog:doc_object_href/2,		% +Object, -HREF
	prolog:doc_object_label_class/3,	% +Object, -Label, -Class
	prolog:ac_object_attributes/2.		% +Object, -Attributes

:- http_handler(root(autocomplete/ac_predicate), ac_predicate,
		[spawn(complete)]).

max_results_displayed(100).

%%	prolog:doc_search_field(+Options) is det.
%
%	Emit the manual-search field.

prolog:doc_search_field(Options) -->
	{ option(id(Id), Options),
	  http_link_to_id(ac_predicate, [], URL)
	},
	html_requires(jquery_ui),
	html([ input(Options, []),
	       script(type('text/javascript'),
		      [ \[ '$(function() {\n',
			   '  $("#',Id,'").autocomplete({\n',
			   '    minLength: 1,\n',
			   '    delay: 0.3,\n',
			   '    source: "',URL,'",\n',
			   '    focus: function(event,ui) {\n',
			   '      $("#',Id,'").val(ui.item.label);\n',
			   '      return false;\n',
			   '    },\n',
			   '    select: function(event,ui) {\n',
			   '      $("#',Id,'").val(ui.item.label);\n',
			   '      window.location.href = ui.item.href;\n',
			   '      return false;\n',
			   '    }\n',
			   '  })\n',
			   '  .data("ui-autocomplete")._renderItem = \c
					  function(ul,item) {\n',
			   '    var label = String(item.label).replace(\n',
			   '		new RegExp(this.term),\n',
			   '		"<span class=\\"acmatch\\">$&</span>");\n',
			   '    var tag = item.tag ? " <i>["+item.tag+"]</i>" : "";\n',
			   '    return $("<li>")\n',
			   '      .append("<a class=\\""+item.class+"\\">"+\c
					  label+tag+\c
					  "</a>")\n',
			   '      .appendTo(ul)\n',
			   '  };\n',
			   '});\n'
			 ]
		      ])
	     ]).


%%	ac_predicate(+Request)
%
%	HTTP handler to reply autocompletion

ac_predicate(Request) :-
	http_parameters(Request,
			[ term(Query, [])
			]),
	max_results_displayed(Max),
	autocompletions(Query, Max, _Count, Completions),
	reply_json(Completions).

autocompletions(Query, Max, Count, Completions)  :-
	autocompletions_by(name, Query, BNC, ByName),
	(   BNC > Max
	->  first_results(Max, ByName, Completions),
	    Count = BNC
	;   autocompletions_by(token, Query, BTC, ByToken),
	    ord_subtract(ByToken, ByName, NewByToken),
	    append(ByName, NewByToken, All),
	    Count is min(Max, BNC+BTC),
	    first_results(Max, All, Completions)
	).

autocompletions_by(How, Query, Count, Completions) :-
	findall(C, ac_object(How, Query, C), Completions0),
	sort(Completions0, Completions),
	length(Completions, Count).

first_results(Max, Completions, Results) :-
	first_n(Max, Completions, FirstN),
	maplist(obj_result, FirstN, Results).

obj_result(_Name-Obj, json([ label=Label,
			     class=Type,
			     href=Href
			   | Extra
			   ])) :-
	obj_name(Obj, Label, Type),
	(   prolog:doc_object_href(Obj, Href)
	->  true
	;   object_href(Obj, Href)
	),
	obj_tag(Obj, Extra).

obj_tag(Object, Extra) :-
	prolog:ac_object_attributes(Object, Extra), !.
obj_tag(Name/Arity, Extra) :-
	current_predicate(system:Name/Arity),
	functor(Head, Name, Arity),
	predicate_property(system:Head, iso), !,
	Extra = [tag=iso].
obj_tag(f(_Func/_Arity), [tag=function]) :- !.
obj_tag(_, []).


%%	obj_name(+Object, -Label, -Class) is det.
%
%	Provide the (autocomplete) label for Object   and its class. The
%	class may be used to style the hit in www/css/plweb.css.

obj_name(Object, Label, Class) :-
	prolog:doc_object_label_class(Object, Label, Class), !.
obj_name(c(Function), Name, cfunc) :- !,
	atom_concat(Function, '()', Name).
obj_name(f(Func/Arity), Name, function) :- !,
	format(atom(Name), '~w/~w', [Func, Arity]).
obj_name(_:Term, Name, pred) :- !,
	format(atom(Name), '~w', [Term]).
obj_name(Name/Arity, Label, Class) :-
	current_predicate(system:Name/Arity),
	functor(Head, Name, Arity),
	predicate_property(system:Head, iso), !,
	format(atom(Label), '~w/~w', [Name, Arity]),
	Class = iso.
obj_name(Term, Name, pred) :-
	format(atom(Name), '~w', [Term]).

first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	first_n(N2, T0, T).


		 /*******************************
		 *	  PREFIX DATABASE	*
		 *******************************/

ac_object(name, Prefix, Name-Obj) :-
	prefix_index(ByName, _ByToken),
	rdf_keys_in_literal_map(ByName, prefix(Prefix), Keys),
	member(Name, Keys),
	name_object(Name, Obj, _Category).
ac_object(token, Prefix, Name-Obj) :-
	prefix_index(_ByName, ByToken),
	rdf_keys_in_literal_map(ByToken, prefix(Prefix), Keys),
	member(Token, Keys),
	rdf_find_literal_map(ByToken, [Token], Names),
	member(Name, Names),
	name_object(Name, Obj, _Category).
ac_object(MatchHow, Term, Match) :-
	prolog:ac_object(MatchHow, Term, Match).


:- dynamic
	prefix_map/2,			% name-map, token-map
	name_object/3.

prefix_index(ByName, ByToken) :-
	with_mutex(autocomplete,
		   create_prefix_index(ByName, ByToken)).

create_prefix_index(ByName, ByToken) :-
	prefix_map(ByName, ByToken), !.
create_prefix_index(ByName, ByToken) :-
	rdf_new_literal_map(ByName),
	rdf_new_literal_map(ByToken),
	assertz(prefix_map(ByName, ByToken)),
	fill_token_map.

fill_token_map :-
	prefix_map(ByName, ByToken),
	rdf_reset_literal_map(ByName),
	rdf_reset_literal_map(ByToken),
	retractall(name_object(_,_,_)),
	(   documented(Obj, Category),
	    completion_target(Obj, Name),
	    assertz(name_object(Name, Obj, Category)),
	    rdf_insert_literal_map(ByName, Name, Name),
	    forall(start_inside_token(Name, Token),
		   rdf_insert_literal_map(ByToken, Token, Name)),
	    fail
	;   true
	),
	keep_best_doc.

documented(Obj, Category) :-
	prolog:doc_object_summary(Obj, Category, _Section, _Summary).

keep_best_doc :-
	(   name_object(Name, Obj, Category),
	    name_object(Name, Obj2, Category2),
	    same_object(Obj, Obj2),
	    better_category(Category2, Category),
	    retract(name_object(Name, Obj, Category)),
	    fail
	;   true
	).

same_object(_:Name/Arity, Name/Arity).
same_object(Name/Arity, _:Name/Arity).

better_category(manual, _) :- !.
better_category(packages, _) :- !.


completion_target(Name/_,   Name).
completion_target(M:Name/A, Name) :-
	functor(Head, Name, A),
	predicate_property(M:Head, exported).
completion_target(f(Name/_),Name).
completion_target(c(Name),  Name).

start_inside_token(Token, Inside) :-
	sub_atom(Token, _, _, L, '_'),
	sub_atom(Token, _, L, 0, Inside).
