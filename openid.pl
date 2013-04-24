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
	    site_user_property/2	% +User, ?Property
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/html_write)).
:- use_module(library(http/recaptcha)).
:- use_module(library(persistency)).
:- use_module(library(settings)).

/** <module> Handle users of the SWI-Prolog website
*/

:- multifile
	http_openid:openid_hook/1.

:- persistent
	openid_user_server(user:atom,
			   server:atom),
	site_user(openid:atom,
		  name:atom,
		  email:atom).

:- initialization
	db_attach('openid.db',
		  [ sync(close)
		  ]).

:- http_handler(root(user/create_profile),  create_profile, []).
:- http_handler(root(user/submit_profile),  submit_profile, []).


		 /*******************************
		 *	    USER ADMIN		*
		 *******************************/

site_user_property(OpenId, name(Name)) :-
	site_user(OpenId, Name, _).
site_user_property(OpenId, email(Email)) :-
	site_user(OpenId, _, Email).


has_profile(OpenId) :-
	site_user(OpenId, _, _), !.

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
	openid_user(Request, User, []),
	(   has_profile(User)
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
	openid_user(Request, User, []),
	http_parameters(Request,
			[ return(Return, [])
			]),
	reply_html_page(
	    wiki,
	    title('Create user profile for SWI-Prolog'),
	    \create_profile(User, Return)).


create_profile(User, Return) -->
	(   { has_profile(User) }
	->  html(h1(class(wiki), 'Update profile'))
	;   html(h1(class(wiki), 'Create profile'))
	),
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
		    table([ tr([th('OpenID'), td(input([ name(openid),
							 value(User),
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
				  input([type(submit), value('Create profile')])))
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
	openid_user(Request, User, []),
	recaptcha_parameters(ReCAPTCHA),
	http_parameters(Request,
			[ name(Name,   [optional(true), default(anonymous)]),
			  email(Email, [optional(true), default('')]),
			  return(Return, [])
			| ReCAPTCHA
			]),
	(   recaptcha_verify(Request, ReCAPTCHA)
	->  retractall_site_user(User, _, _),
	    assert_site_user(User, Name, Email),
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


		 /*******************************
		 *     OPENID CUSTOMIZATION	*
		 *******************************/

http_openid:openid_hook(trusted(OpenId, Server)) :-
	openid_user_server(OpenId, Server), !.
http_openid:openid_hook(trusted(OpenId, Server)) :-
	assert_openid_user_server(OpenId, Server), !.

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
			[ \openid_login_form(ReturnTo, []),
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
