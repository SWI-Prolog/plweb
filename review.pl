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

:- use_module(markitup).
:- use_module(rating).
:- use_module(openid).

:- http_handler(root(pack/review),        pack_review,        []).
:- http_handler(root(pack/review/submit), pack_submit_review, []).
:- http_handler(root(pack/review/rating), pack_rating,        []).

%%	pack_review(+Request)
%
%	HTTP handler to review a pack.

pack_review(Request) :-
	openid_user(Request, OpenId, []), !,
	http_parameters(Request,
			[ p(Pack, [])
			]),
	http_link_to_id(pack_submit_review, [], Action),
	reply_html_page(wiki,
			title('Review pack ~w'-[Pack]),
			[ h1('Review pack ~w'-[Pack]),
			  \explain(Pack, OpenId),
			  form([ class(review), action(Action) ],
			       [ input([type(hidden), name(p), value(Pack)]),
				 input([type(hidden), name(rating), value(-1)]),
				 \rating(Pack),
				 \comment(Pack),
				 input([type(submit), value('Submit review')])
			       ])
			]).


explain(Pack, _User) -->
	html([ p('You requested to review pack ~w'-[Pack])
	     ]).

rating(Pack) -->
	{ http_link_to_id(pack_rating, [], HREF)
	},
	rate([ on_rating(HREF),
	       data_id(Pack),
	       set_field(rating),
	       rate_max(5),
	       step(true),
	       type(big),
	       can_rate_again(true)
	     ]).

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
	markitup([ id(comment),
		   markup(pldoc),
		   cold(60),
		   rows(10)
		 ]).


%%	pack_submit_review(+Request)
%
%	Handle a pack review submission

pack_submit_review(Request) :-
	openid_user(Request, User, []),
	http_parameters(Request,
			[ p(Pack, []),
			  rating(Rating, []),
			  comment(Comment, [optional(true), default('')])
			]),
	reply_html_page(wiki,
			title('Thanks for your review of ~w'-[Pack]),
			[ table([ tr([th('User:'), td(User)]),
				  tr([th('Rating:'), td(Rating)]),
				  tr([th('Comment:'), td(Comment)])
				])
			]).
