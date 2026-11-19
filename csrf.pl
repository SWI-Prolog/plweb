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


:- module(csrf,
          [ csrf_check/1                % +Request
          ]).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(debug)).

/** <module> Protect against cross-site request forgery

Handlers that change  state  on  behalf  of   a  logged  in  user  call
csrf_check/1 before doing anything. We do not use tokens, but verify the
request was initiated from  our  own  site.   Modern  browsers  send the
``Sec-Fetch-Site`` header, which cannot be set by scripts.  For browsers
that do not, we fall back to the ``Origin`` or ``Referer`` header.

Requests without any of these headers are refused.  Note that this
implies that state-changing links cannot be used from bookmarks or
mail, as browsers send ``Sec-Fetch-Site: none`` for these.
*/

%!  csrf_check(+Request) is det.
%
%   Raise an HTTP 403 (Forbidden) reply if Request was not initiated
%   from our own site.

csrf_check(Request) :-
    same_site_request(Request),
    !.
csrf_check(Request) :-
    memberchk(path(Path), Request),
    debug(csrf, 'CSRF: refused ~q', [Request]),
    throw(http_reply(forbidden(Path))).

same_site_request(Request) :-
    memberchk(sec_fetch_site(Site), Request),
    !,
    memberchk(Site, ['same-origin', 'same-site']).
same_site_request(Request) :-
    (   memberchk(origin(URL), Request)
    ->  true
    ;   memberchk(referer(URL), Request)
    ),
    url_host(URL, Host),
    request_host(Request, Host).

url_host(URL, Host) :-
    atom(URL),
    uri_components(URL, Components),
    uri_data(authority, Components, Authority),
    atom(Authority),
    uri_authority_components(Authority, AuthComponents),
    uri_authority_data(host, AuthComponents, Host0),
    atom(Host0),
    downcase_atom(Host0, Host).

%   request_host(+Request, ?Host)
%
%   Host is the host the client used to address us.  Behind a proxy
%   this may be in X-Forwarded-Host (a comma-separated list if the
%   request passed multiple proxies; the first is the client's).

request_host(Request, Host) :-
    (   memberchk(x_forwarded_host(Hosts), Request)
    ->  atomic_list_concat([First|_], ',', Hosts),
        normalize_space(atom(Host0), First)
    ;   memberchk(host(Host0), Request)
    ),
    downcase_atom(Host0, Host).
