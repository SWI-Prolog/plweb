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

:- module(register, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(openid).
:- use_module(library(smtp)).

:- http_handler(root(register),            register,            []).
:- http_handler(root(submit_registration), submit_registration, []).

register(Request) :-
	site_user(Request, User),
	reply_html_page(title('Register to edit SWI-Prolog wiki pages'),
			\reg_body(User)).

reg_body(User) -->
	{ Form = \form(User)
	},
	html({|html(Form)||
	      <h1 class="wiki">Register to edit SWI-Prolog wiki pages</h1>

	      <p>This form allows you to request an account for editing
	      the SWI-Prolog wiki pages.  That is, the pages that have,
	      an edit button at the top-right. Such pages are stored as
	      PlDoc wiki text.</p>

	      <h2 class="wiki">Registration form</h2>
	      <div>Form</div>
	      |}).

form(UUID) -->
	{ http_location_by_id(submit_registration, Action),
	  PlaceHolder = 'Please tell us your plans, so that we can \c
	  tell you are a genuine human Prolog user',
	  site_user_property(UUID, name(Name), 'anonymous'),
	  site_user_property(UUID, email(Email), 'unknown')
	},
	html(form(action(Action),
		  table([ tr([ th([align(right)], 'Name'),
			       td(input([name(name),
					 placeholder('Name associated to commits'),
					 disabled(disabled),
					 value(Name)
					]))
			     ]),
			  tr([ th([align(right)], 'Email'),
			       td(input([name(email),
					 placeholder('Displayed with GIT commit'),
					 disabled(disabled),
					 value(Email)
					]))
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
	site_user(Request, UUID),
	http_parameters(Request,
			[ comment(Comment, [optional(true)])
			]),
	(   mail(UUID, Comment),
	    reply_html_page(title('Mail sent'),
			    [ h1(class(wiki), 'Mail sent'),
			      p([ 'A mail has been sent to the site adminstrator. ',
				  'You will be informed when the account has been ',
				  'created.'
				])
			    ])
	).

mail(UUID, Comment) :-
	smtp_send_mail('jan@swi-prolog.org',
		       message(UUID, Comment),
		       [ subject('SWI-Prolog wiki edit request'),
			 from('jan@swi-prolog.org')
		       ]).

message(UUID, Comment, Out) :-
	site_user_property(UUID, name(Name), 'anonymous'),
	site_user_property(UUID, email(EMail), 'unknown'),
	format(Out, 'New wiki edit request\n\n', []),
	format(Out, '\t  UUID: ~w~n', [UUID]),
	format(Out, '\t  Name: ~w~n', [Name]),
	format(Out, '\tE-Mail: ~w~n', [EMail]),
	format(Out, '~n~w~n', [Comment]).


site_user_property(UUID, P, Default) :-
	(   site_user_property(UUID, P)
	->  true
	;   arg(1, P, Default)
	).
