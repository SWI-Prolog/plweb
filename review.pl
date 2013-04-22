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

:- module(pack_review, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(persistency)).

:- use_module(markitup).
:- use_module(rating).
:- use_module(openid).

:- http_handler(root(pack/review),        pack_review,        []).
:- http_handler(root(pack/review/submit), pack_submit_review, []).
:- http_handler(root(pack/review/rating), pack_rating,        []).


		 /*******************************
		 *	       DATA		*
		 *******************************/

:- persistent
	review(pack:atom,
	       openid:atom,
	       time:number,
	       rating:integer,
	       comment:atom),
	author(openid:atom,
	       name:atom,
	       email:atom).


:- db_attach(reviews, []).


		 /*******************************
		 *	     INTERFACE		*
		 *******************************/

%%	pack_review(+Request)
%
%	HTTP handler to review a pack.

pack_review(Request) :-
	openid_user(Request, OpenId, []), !,
	http_parameters(Request,
			[ p(Pack, [])
			]),
	http_link_to_id(pack_submit_review, [], Action),
	reply_html_page(
	    wiki,
	    title('Review pack ~w'-[Pack]),
	    [ h1('Review pack ~w'-[Pack]),
	      \explain(Pack, OpenId),
	      form([ class(review), action(Action) ],
		   [ input([type(hidden), name(p), value(Pack)]),
		     table([ \reviewer(OpenId),
			     \rating(Pack, OpenId),
			     \comment(Pack),
			     tr(td([colspan(2), align(right)],
				   input([ type(submit),
					   value('Submit review')
					 ])))
			   ])
		   ])
	    ]).


explain(Pack, _User) -->
	html([ p('You requested to review pack ~w'-[Pack])
	     ]).

%%	reviewer(+OpenID)// is det.
%
%	Present details about the reviewer

reviewer(OpenId) -->
	{ (   author(OpenId, Name, Email)
	  ->  true
	  ;   Name = '',
	      Email = ''
	  )
	}, !,
	html([ tr([th('Name:'),   td(input([ name(name),
					     value(Name),
					     placeholder('Your (nick) name')
					   ]))]),
	       tr([th('E-Mail:'), td(input([ name(email),
					     value(Email),
					     placeholder('Your E-mail')
					   ]))])
	     ]).


rating(Pack, OpenId) -->
	{ http_link_to_id(pack_rating, [], HREF),
	  (   review(Pack, OpenId, _, Rating0, _)
	  ->  Extra = [data_average(Rating0)]
	  ;   Extra = [],
	      Rating0 = -1
	  )
	},
	html(tr([ th('Your rating for ~w'-[Pack]),
		  td( [ input([type(hidden), name(rating), value(Rating0)]),
			\rate([ on_rating(HREF),
				data_id(Pack),
				set_field(rating),
				rate_max(5),
				step(true),
				type(big),
				can_rate_again(true)
			      | Extra
			      ])
		      ])
		])).


%%	pack_rating(+Request)
%
%	Handle the actual rating

pack_rating(Request) :-
	http_parameters(Request,
			[ action(Action, []),
			  rate(Rate, [number])
			], []),
	debug(rating, 'Got Action = ~q, Rate = ~q', [Action,Rate]),
	format('Content-type: text/plain\n\n'),
	format('true\n').


comment(_Pack) -->
	html(tr(td(colspan(2),
		   \markitup([ id(comment),
			       markup(pldoc),
			       cold(60),
			       rows(10)
			     ])))).


%%	pack_submit_review(+Request)
%
%	Handle a pack review submission

pack_submit_review(Request) :-
	openid_user(Request, OpenID, []),
	http_parameters(Request,
			[ p(Pack, []),
			  rating(Rating, [number]),
			  name(Name, []),
			  email(Email, [optional(true), default('')]),
			  comment(Comment, [optional(true), default('')])
			]),
	reply_html_page(
	    wiki,
	    title('Thanks for your review of ~w'-[Pack]),
	    [ \update_user(OpenID, Name, Email),
	      \update_review(Pack, OpenID, Rating, Comment)
	    ]).


%%	update_user(+OpenID, +Name, +Email)// is det.
%
%	Assert/update identity information about the user.

update_user(OpenId, Name, Email) -->
	{ author(OpenId, Name, Email) }, !.
update_user(OpenId, Name, Email) -->
	{ author(OpenId, _, _), !,
	  retractall_author(OpenId, Name, Email),
	  assert_author(OpenId, Name, Email)
	},
	html([ h4(class(wiki), 'Updated user details'),
	       \user_details(OpenId)
	     ]).
update_user(OpenId, Name, Email) -->
	{ assert_author(OpenId, Name, Email)
	},
	html([ h4(class(wiki), 'Stored user details'),
	       \user_details(OpenId)
	     ]).

user_details(OpenID) -->
	{ author(OpenID, Name, Email) },
	html(table([ tr([th('OpenID'), td(OpenID)]),
		     tr([th('Name'),   td(Name)]),
		     tr([th('Email'),  td(Email)])
		   ])).


%%	update_review(+Pack, +OpenID, +Rating, +Comment)// is det.
%
%	Assert/update a review about a pack.

update_review(Pack, OpenID, Rating, Comment) -->
	{ review(Pack, OpenID, _Time, Rating, Comment) }, !,
	html(h4('Review was not updated')),
	show_review(Pack, OpenID).
update_review(Pack, OpenID, Rating, Comment) -->
	{ review(Pack, OpenID, _Time, _Rating, _Comment), !,
	  retractall_review(Pack, OpenID, _, _, _),
	  get_time(Time),
	  assert_review(Pack, OpenID, Time, Rating, Comment)
	},
	html(h4('Updated review for pack ~w'-[Pack])),
	show_review(Pack, OpenID).
update_review(Pack, OpenID, Rating, Comment) -->
	{ get_time(Time),
	  assert_review(Pack, OpenID, Time, Rating, Comment)
	},
	html(h4('Added review for pack ~w'-[Pack])),
	show_review(Pack, OpenID).


%%	show_review(+Pack, +OpenID)// is det.
%
%	Show an individual review about Pack

show_review(Pack, OpenID) -->
	{ review(Pack, OpenID, _Time, Rating, Comment) },
	html([ table([ tr([th('User:'), td(OpenID)]),
		       tr([th('Rating:'), td(Rating)]),
		       tr([th('Comment:'), td(Comment)])
		     ])
	     ]).

