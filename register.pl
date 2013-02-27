/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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

:- module(register, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(smtp)).

:- http_handler(root(register),            register,            []).
:- http_handler(root(submit_registration), submit_registration, []).

register(_Request) :-
	reply_html_page(title('Register to edit SWI-Prolog wiki pages'),
			[ h1(class(wiki),
			     'Register to edit SWI-Prolog wiki pages'),
			  \reg_body
			]).

reg_body -->
	html([ p([ 'This form allows you to request an account for editing ',
		   'the SWI-Prolog wiki pages.  That is, the pages that have ',
		   'an edit button at the top-right. ',
		   'Such pages are stored as PlDoc wiki text.  Editing requires ',
		   'you to be familiar with PlDoc. '
		 ]),
	       h2(class(wiki), 'Registration form'),
	       \form,
	       h2(class(wiki), 'Privacy'),
	       p([ 'The information provided is sent to the site maintainer. ',
		   'Your password is sent as an MD5 hash. ',
		   'If you login, the password is exchanged over an unprotected ',
		   'HTTP connection. Please do use a fair password that is not ',
		   'used on critical sites.'
		 ])
	     ]).

form -->
	{ http_location_by_id(submit_registration, Action),
	  PlaceHolder = 'Please tell us your plans, so that we can \c
	  tell you are a genuine human Prolog user'
	},
	html(form(action(Action),
		  table([ tr([ th([align(right)], 'Nick name:'),
			       td(input([name(login),
					 placeholder('Use this for login')
					]))
			     ]),
			  tr([ th([align(right)], 'Real name:'),
			       td(input([name(name),
					 placeholder('Displayed with GIT commit')
					]))
			     ]),
			  tr([ th([align(right)], 'Email:'),
			       td(input([name(email),
					 placeholder('Displayed with GIT commit')
					]))
			     ]),
			  tr([ th([align(right)], 'Password:'),
			       td(input([name(passwd1), type(password)]))
			     ]),
			  tr([ th([align(right)], 'Retype:'),
			       td(input([name(passwd2), type(password)]))
			     ]),
			  tr([ th([align(right), valign(top)], 'Comments:'),
			       td([ class(wiki_text), colspan(2) ],
				  textarea([ cols(50),rows(10),name(comment),
					     placeholder(PlaceHolder)
					   ],
					   ''))
			     ]),
			  tr([ td([ colspan(2), align(right) ],
				  input([type(submit)]))
			     ])
			]))).


%%	submit_registration(+Request) is det.
%
%	Sent E-mail to submit a registration

submit_registration(Request) :-
	http_parameters(Request,
			[ login(Login, [length>=3]),
			  name(Name, [length>=3]),
			  email(Email, [length>=3]),
			  passwd1(Pwd1, [length>=6]),
			  passwd1(Pwd2, [length>=6]),
			  comment(Comment, [optional(true)])
			]),
	(   check_login(Login)
	;   check_passwd(Pwd1, Pwd2)
	;   mail(Login, Name, Email, Pwd1, Comment),
	    reply_html_page(title('Mail sent'),
			    [ h1(class(wiki), 'Mail sent'),
			      p([ 'A mail has been sent to the site adminstrator. ',
				  'You will be informed when the account has been ',
				  'created.'
				])
			    ])
	).

check_login(Login) :-
	http_current_user(passwd, Login, _), !,
	reply_html_page(title('Existing login'),
			[ h1(class(wiki), 'Existing login'),
			  p([ 'Login ~w is already in use. '-[Login]
			    ])
			]).

check_passwd(Pwd1, Pwd2) :-
	Pwd1 \== Pwd2, !,
	reply_html_page(title('Password mismatch'),
			[ h1(class(wiki), 'Password mismatch'),
			  p([ 'The password and retyped passwords are not ',
			      'identical.'
			    ])
			]).

mail(Login, Name, Email, Password, Comment) :-
	smtp_send_mail('jan@swi-prolog.org',
		       message(Login, Name, Email, Password, Comment),
		       [ subject('SWI-Prolog wiki edit request'),
			 from('jan@swi-prolog.org')
		       ]).

message(Login, Name, Email, Password, Comment, Out) :-
	append("$1$", _, Hash),
	crypt(Password, Hash),
	format(Out, 'New wiki edit request\n\n', []),
	format(Out, '\t~w:~s:~w:~w~n~n', [Login, Hash, Name, Email]),
	format(Out, '~w~n', [Comment]).
