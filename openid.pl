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

:- module(plweb_openid,
	  [ site_user/2,		% +Request, -User
	    site_user_property/2,	% +User, ?Property
	    current_user//0
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_header)).
:- use_module(library(http/html_write)).
:- use_module(library(http/recaptcha)).
:- use_module(library(http/http_stream)).
:- use_module(library(persistency)).
:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(library(uuid)).

:- use_module(review).
:- use_module(pack).

/** <module> Handle users of the SWI-Prolog website
*/

:- multifile
	http_openid:openid_hook/1.

:- persistent
	openid_user_server(user:atom,
			   server:atom),
	site_user(uuid:atom,
		  openid:atom,
		  name:atom,
		  email:atom),
	stay_signed_in(openid:atom,
		       cookie:atom,
		       peer:atom,
		       time:integer,
		       expires:integer).

:- initialization
	db_attach('openid.db',
		  [ sync(close)
		  ]).

:- http_handler(root(user/create_profile),  create_profile, []).
:- http_handler(root(user/submit_profile),  submit_profile, []).
:- http_handler(root(user/logout),	    logout,         []).
:- http_handler(root(user/profile/public),  public_profile, []).


		 /*******************************
		 *	    USER ADMIN		*
		 *******************************/

site_user_property(UUID, openid(OpenId)) :-
	site_user(UUID, OpenId, _, _).
site_user_property(UUID, name(Name)) :-
	site_user(UUID, _, Name, _).
site_user_property(UUID, email(Email)) :-
	site_user(UUID, _, _, Email).


has_profile(OpenId) :-
	site_user_property(_, openid(OpenId)).

		 /*******************************
		 *	 USER INTERACTION	*
		 *******************************/

:- multifile recaptcha:key/2.

:- setting(recaptcha:public_key, atom, '',
	   'reCAPTCHA public key').
:- setting(recaptcha:private_key, atom, '',
	   'reCAPTCHA private key').

recaptcha:key(public,  Key) :- setting(recaptcha:public_key,  Key).
recaptcha:key(private, Key) :- setting(recaptcha:private_key, Key).

%%	site_user(+Request, -User)
%
%	Demand the user to be logged on and, if this is the first logon,
%	verify the user and create a profile.

site_user(Request, User) :-
	openid_user(Request, OpenID, []),
	(   site_user_property(User, openid(OpenID))
	->  true
	;   option(request_uri(RequestURI), Request),
	    http_link_to_id(create_profile, [return(RequestURI)], HREF),
	    http_redirect(moved_temporary, HREF, Request)
	).


%%	create_profile(+Request).
%
%	Create a new user profile, and on success return to the original
%	location.

create_profile(Request) :-
	openid_user(Request, OpenID, []),
	http_parameters(Request,
			[ return(Return, [])
			]),
	reply_html_page(
	    wiki,
	    title('Create user profile for SWI-Prolog'),
	    \create_profile(OpenID, Return)).


create_profile(OpenID, Return) -->
	{ (   site_user_property(User, openid(OpenID))
	  ->  Op = 'Update profile'
	  ;   uuid(User),		% new user
	      Op = 'Create profile'
	  )
	},
	html(h1(class(wiki), Op)),
	{ http_link_to_id(submit_profile, [], Action),
	  (   site_user_property(User, name(Name))
	  ->  true
	  ;   Name = ''
	  ),
	  (   site_user_property(User, email(Email))
	  ->  true
	  ;   Email = ''
	  )
	},
	html(form([ class(create_profile), method('POST'), action(Action) ],
		  [ input([type(hidden), name(return), value(Return)]),
		    input([type(hidden), name(uuid), value(User)]),
		    table([ tr([th('OpenID'), td(input([ name(openid),
							 value(OpenID),
							 disabled(disabled)
						       ]))]),
			    tr([th('Name'),   td(input([ name(name),
							 value(Name),
							 placeholder('Displayed name')
						       ]))]),
			    tr([th('Email'),  td(input([ name(email),
							 value(Email),
							 placeholder('Your E-mail address')
						       ]))]),
			    tr(td(colspan(2), \recaptcha([]))),
			    tr(td([colspan(2), align(right)],
				  input([type(submit), value(Op)])))
			  ])
		  ])),
	expain_create_profile.


expain_create_profile -->
	html(div(class('openid-explanation'),
		 [ p([ 'On this page, we ask you to proof you are human and ',
		       'create a minimal profile. '  ,
		       'Your name is displayed along with comments that you create. ',
		       'Your E-mail will only be used for communication by the site ',
		       'administration in -currently- unforeseen circumstances. ',
		       'Notably, it will not be displayed, not be used for spamming and ',
		       'not be handed to third parties.'
		     ])
		 ])).


%%	submit_profile(+Request)
%
%	Handle submission of the user profile

submit_profile(Request) :-
	openid_user(Request, OpenID, []),
	recaptcha_parameters(ReCAPTCHA),
	http_parameters(Request,
			[ uuid(User,   []),
			  name(Name,   [optional(true), default(anonymous)]),
			  email(Email, [optional(true), default('')]),
			  return(Return, [])
			| ReCAPTCHA
			]),
	(   recaptcha_verify(Request, ReCAPTCHA)
	->  retractall_site_user(User, OpenID, _, _),
	    assert_site_user(User, OpenID, Name, Email),
	    http_redirect(moved_temporary, Return, Request)
	;   reply_html_page(
		wiki,
		title('CAPTCHA failed'),
		[ h1(class(wiki), 'CAPTCHA verification failed'),
		  p([ 'Please use the back button of your browser and ',
		      'try again'
		    ])
		])
	).

%%	public_profile(+Request) is det.
%
%	HTTP handler showing the public profile for a user.

public_profile(Request) :-
	http_parameters(Request,
			[ user(UUID, [])
			]),
	site_user_property(UUID, name(Name)),
	reply_html_page(
	    wiki,
	    title('User ~w'-[Name]),
	    [ h1(class(wiki), 'Public info for user ~w'-[Name]),
	      \public_profile(UUID)
	    ]).

public_profile(UUID) -->
	user_packs(UUID),
	profile_reviews(UUID).

user_packs(UUID) -->
	{ setof(Pack, current_pack([author(UUID)], Pack), Packs), !,
	  sort_packs(rating, Packs, Sorted),
	  site_user_property(UUID, name(Name))
	},
	html([ h2(class(wiki), 'Packages by ~w'-[Name])
	     ]),
	pack_table(Sorted, []).
user_packs(_) -->
	[].


		 /*******************************
		 *     OPENID CUSTOMIZATION	*
		 *******************************/

stay_login_cookie(swipl_login).

http_openid:openid_hook(trusted(OpenId, Server)) :-
	openid_user_server(OpenId, Server), !.
http_openid:openid_hook(trusted(OpenId, Server)) :-
	assert_openid_user_server(OpenId, Server), !.
http_openid:openid_hook(stay_signed_in(OpenId)) :-
	assertion(in_header_state),
	http_session_cookie(Cookie),
	get_time(NowF),
	Now is round(NowF),
	http_current_request(Request),
	http_peer(Request, Peer),
	Expires is Now+31*24*60*60,	% 31 days from now
	assert_stay_signed_in(OpenId, Cookie, Peer, Now, Expires),
	http_session_option(path(Path)),
	debug(openid(stay_signed_in),
	      'Created stay-signed-in for ~q', [OpenId]),
	http_timestamp(Expires, RFC1123),
	stay_login_cookie(CookieName),
	format('Set-Cookie: ~w=~w; Expires=~w; path=~w\r\n',
	       [CookieName, Cookie, RFC1123, Path]).
http_openid:openid_hook(logout(OpenId)) :-
	nonvar(OpenId),
	assertion(in_header_state),
	retractall_stay_signed_in(OpenId, _, _, _, _),
	http_session_option(path(Path)),
	stay_login_cookie(CookieName),
	format('Set-Cookie: ~w=; \c
	        expires=Tue, 01-Jan-1970 00:00:00 GMT; \c
		path=~w\r\n',
	       [CookieName, Path]),
	fail.
http_openid:openid_hook(logged_in(OpenId)) :-
	(   http_in_session(_),
	    http_session_data(openid(OpenId))
	->  true
	;   http_current_request(Request),
	    memberchk(cookie(Cookies), Request),
	    memberchk(swipl_login=Cookie, Cookies),
	    stay_signed_in(OpenId, Cookie, _Peer, _Time, _Expires)
	->  http_open_session(_, []),
	    http_session_assert(openid(OpenId)),
	    debug(openid(stay_signed_in),
		  'Granted stay-signed-in for ~q', [OpenId])
	).


in_header_state :-
	current_output(CGI),
	cgi_property(CGI, state(header)), !.

:- http_handler(openid(login),  plweb_login_page, []).

%%	plweb_login_page(+Request)
%
%	HTTP handler that  overrules  the   location  openid(login)  for
%	customizating the -very basic- login page.

plweb_login_page(Request) :-
	http_open_session(_, []),
	http_parameters(Request,
			[ 'openid.return_to'(ReturnTo, [])
			]),
	reply_html_page(wiki,
			[ title('SWI-Prolog login')
			],
			[ \openid_login_form(ReturnTo,
					     [ show_stay(true)
					     ]),
			  \explain
			]).

explain -->
	html([ div(class('openid-explanation'),
		   [ p([ 'Unfortunately, we have to take some measures to avoid ',
			 'abuse of this service.  We demand login using ',
			 a(href('http://openid.net/'), 'OpenID'), '. ',
			 'Currently, we accept any OpenID provider.'
		       ]),
		     p([ 'After logging in for the first time, we will ask for ',
			 'some additional information.  All information is ',
			 'optional.'
		       ])
		   ])
	     ]).


		 /*******************************
		 *	      LOGOUT		*
		 *******************************/

%%	logout(+Request)
%
%	Logout the current user and reload the current page.

logout(_Request) :-
	openid_logged_in(OpenId), !,
	openid_logout(OpenId),
	reply_html_page(
	    wiki,
	    title('Logged out'),
	    [ h1(class(wiki), 'Logout'),
	      p('Thanks for using www.swi-prolog.org')
	    ]).
logout(_Request) :-
	reply_html_page(
	    wiki,
	    title('Not logged in'),
	    [ h1(class(wiki), 'Logout'),
	      p(class(warning), 'You were not logged in')
	    ]).


%%	current_user//

current_user -->
	{ openid_logged_in(OpenID),
	  (   site_user_property(User, openid(OpenID)),
	      site_user_property(User, name(Name)),
	      Name \== ''
	  ->  Display = Name
	  ;   Display = OpenID
	  ),
	  http_link_to_id(logout, [], Logout)
	},
	html(div(class('current-user'),
		 [ Display,
		   ' (', a([class(logout), href(Logout)], 'logout'), ')'
		 ])).
current_user -->
	[].


