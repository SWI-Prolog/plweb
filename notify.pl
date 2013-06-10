/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

:- module(mail_notify,
	  [ notify/2				% +Object, +Term
	  ]).
:- use_module(library(smtp)).
:- use_module(openid).
:- use_module(tagit, [object_label/2]).
:- use_module(library(pldoc/doc_html), [object_href/2]).

:- multifile
	event_subject//1,			% +Event
	event_message//1.			% +Event

/** <module> Send notications by E-mail

This module sends E-mail notifications  to   _watchers_  for events that
take place on watched objects.  The   messages  themselves are generated
similar to print_message/2 using the grammars

  - mail_notify:event_subject//1
    Define the subject of the message
  - mail_notify:event_message//1
    Define the body of the message

@tbd	Eventually, this may also be used to provide an RSS feed from
	the side.
*/

%%	notify(+Object, +Term)
%
%	Notify watching users by mail of  the event on Object, described
%	by Term.

notify(Object, Term) :-
	catch(thread_send_message(mail_notifier, notification(Object, Term)),
	      error(existence_error(message_queue(_,_),_)),
	      start_notifier(Object, Term)).


start_notifier(Object, Term) :-
	thread_create(mail_notifier, _, [alias(mail_notifier)]),
	thread_send_message(mail_notifier, notification(Object, Term)).

mail_notifier :-
	repeat,
	thread_get_message(Msg),
	catch(handle_message(Msg), E,
	      print_message(error, E)),
	fail.

handle_message(notification(Object, Term)) :- !,
	notify(Object, Term).
handle_message(Message) :-
	domain_error(notification, Message).

notify(Object, Term) :-
	(   watcher(Object, Watcher),
	    (	site_user_property(Watcher, email(Email)),
		User = Watcher
	    ;	site_user_property(User, email(Watcher)),
		Email = Watcher
	    ),
	    catch(notify(User, Email, Object, Term),
		  E,
		  print_message(error, E)),
	    fail
	;   true
	).

notify(User, Email, Object, Term) :-
	phrase(make_subject(Object, Term), SubjectList),
	phrase(make_message(User, Object, Term), Message),
	with_output_to(atom(Subject),
		       send_message(SubjectList, current_output)),
	smtp_send_mail(Email,
		       send_message(Message),
		       [ subject(Subject),
			 from('noreply@swi-prolog.org')
		       ]).

%%	send_message(+Parts, +Output) is det.
%
%	Write  message  fragments  to  Output.    This   is  similar  to
%	print_message/2.

send_message([], _) :- !.
send_message([H|T], Out) :- !,
	send_one(H, Out),
	send_message(T, Out).

send_one(Fmt-Args, Out) :- !,
	format(Out, Fmt, Args).
send_one(nl, Out) :- !,
	format(Out, '~n', []).
send_one(X, _Out) :- !,
	domain_error(mail_message_fragment, X).

%%	make_subject(+Object, +Event)//
%
%	Generate the fragments that describe the   subject  for Event on
%	Object.

make_subject(Object, Event) -->
	{ object_label(Object, Label) },
	[ '[SWIPL] ~w: '-[Label] ],
	(   event_subject(Event)
	->  []
	;   ['<unknown event>'-[]]
	).

%%	make_message(+UUID, +Object, +Event)//
%
%	Generate the fragments that describe the  message body for Event
%	on Object.

make_message(UUID, Object, Event) -->
	opening(UUID),
	on_object(Object),
	(   event_message(Event)
	->  []
	;   ['Unknown notication event: ~q'-[Event] ]
	),
	closing(Object).

opening(UUID) -->
	{ site_user_property(UUID, name(Name)) },
	[ 'Dear ~w,'-[Name], nl, nl ].
opening(_) -->
	[ 'Hi'-[Name], nl, nl ].

on_object(Object) -->
	{ object_label(Object, Label),
	  object_href(Object, HREF)
	},
	[ 'This is a change notification for ~w'-[Label], nl,
	  'URL: http://www.swi-prolog.org~w'-[HREF], nl, nl
	].

closing(_Object) -->
	[ nl, nl,
	  'You received this message because you have indicated to '-[], nl,
	  'watch this page on the SWI-Prolog website.'-[], nl
	].


		 /*******************************
		 *	    WATCH LIST		*
		 *******************************/

%%	watcher(+Object, -MailOrUser) is nondet.
%
%	True when Object is being watched by MailOrUser. Note that users
%	are described by their UUID,  and   thus  never  conflict with a
%	valid mail address.
%
%	@tbd:	Allow users to set watches

watcher(_, 'J.Wielemaker@vu.nl').
