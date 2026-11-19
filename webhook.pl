/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (C): 2026, SWI-Prolog Solutions b.v.

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


:- module(webhook,
          [ webhook_authorized/1        % +Request
          ]).
:- use_module(library(settings)).
:- use_module(library(sha)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(openid).
:- use_module(csrf).

/** <module> Authorize GitHub webhook requests

Handlers such as /blog/pull and /examples/pull run `git pull`, rebuild
indexes and purge the CDN.  They may only be triggered by

  - GitHub, using a webhook with a shared secret.  The request must
    carry a valid ``X-Hub-Signature-256`` header: an HMAC-SHA256 of the
    request body using the secret.  The secret is the setting
    `webhook:secret`, set in `data/private/plweb.conf` as

        setting(webhook:secret, 'the secret').

    and in the GitHub webhook configuration (Settings/Webhooks/Secret).
  - A logged in admin, from a page on this site.

Anything else is refused with 403 (Forbidden).
*/

:- setting(secret, atom, '',
           'Shared secret for GitHub webhooks (/blog/pull, /examples/pull)').

max_body_size(1_000_000).

%!  webhook_authorized(+Request) is det.
%
%   Succeed if Request may trigger a webhook action.  Otherwise
%   throw an HTTP 403 reply.  For signed requests the body is read.

webhook_authorized(Request) :-
    memberchk(x_hub_signature_256(Signature), Request),
    !,
    (   setting(secret, Secret),
        Secret \== '',
        read_body(Request, Body),
        hmac_sha(Secret, Body, HMAC, [ algorithm(sha256),
                                       encoding(octet)
                                     ]),
        hash_atom(HMAC, Hex),
        atom_concat('sha256=', Hex, Expected),
        same_string(Expected, Signature)
    ->  debug(webhook, 'Valid webhook signature', [])
    ;   refuse(Request)
    ).
webhook_authorized(Request) :-
    site_user_logged_in(User),
    site_user_property(User, granted(admin)),
    !,
    csrf_check(Request).
webhook_authorized(Request) :-
    refuse(Request).

refuse(Request) :-
    memberchk(path(Path), Request),
    throw(http_reply(forbidden(Path))).

%!  read_body(+Request, -Body:string) is semidet.
%
%   Read the raw request body as a string of bytes (octets).

read_body(Request, Body) :-
    memberchk(content_length(Len), Request),
    max_body_size(Max),
    Len =< Max,
    memberchk(input(In), Request),
    (   stream_property(In, encoding(octet))
    ->  true
    ;   set_stream(In, encoding(octet))
    ),
    read_string(In, Len, Body),
    string_length(Body, Len).

%!  same_string(+A, +B) is semidet.
%
%   Compare in time that only depends on the length, such that the
%   signature cannot be guessed from response times.

same_string(A, B) :-
    atom_codes(A, CA),
    atom_codes(B, CB),
    length(CA, Len),
    length(CB, Len),
    foldl(xor_or, CA, CB, 0, Diff),
    Diff == 0.

xor_or(A, B, D0, D) :-
    D is D0 \/ (A xor B).
