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


:- module(public_url,
          [ public_url/1,               % +URL
            must_be_public_url/1        % +URL
          ]).
:- use_module(library(uri)).
:- use_module(library(socket)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Check URLs fetched on behalf of users

The server fetches URLs provided by users and pack authors: pack
archives and git repositories and OpenID discovery documents.  This
module verifies such URLs are http(s) URLs for hosts that only resolve
to public addresses, such that they cannot be used to access services
on the server itself or the internal network (Server Side Request
Forgery).

Note that this does not protect against redirects to internal
addresses or DNS rebinding.
*/

%!  public_url(+URL) is semidet.
%
%   True if URL is an http or https URL whose host resolves only to
%   public addresses.

public_url(URL) :-
    atom(URL),
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    memberchk(Scheme, [http, https]),
    uri_data(authority, Components, Authority),
    atom(Authority),
    uri_authority_components(Authority, AC),
    uri_authority_data(host, AC, Host0),
    atom(Host0),
    Host0 \== '',
    host_name(Host0, Host),
    catch(findall(Addr,
                  ( host_address(Host, SockAddr, [type(stream)]),
                    get_dict(address, SockAddr, Addr)
                  ),
                  Addrs),
          _, fail),
    Addrs \== [],
    maplist(public_address, Addrs).

%   host_name(+URIHost, -Host)
%
%   Remove the brackets from an IPv6 address literal.

host_name(Host0, Host) :-
    atom_concat('[', Rest, Host0),
    atom_concat(Host, ']', Rest),
    !.
host_name(Host, Host).

%!  must_be_public_url(+URL) is det.
%
%   @error domain_error(public_url, URL)

must_be_public_url(URL) :-
    public_url(URL),
    !.
must_be_public_url(URL) :-
    domain_error(public_url, URL).

%!  public_address(+Address) is semidet.
%
%   True if Address is not a loopback, private, link-local, multicast
%   or otherwise reserved address.

public_address(ip(A,B,C,D)) :-
    !,
    \+ private_ip4(A,B,C,D).
public_address(ip(A,B,C,D,E,F,G,H)) :-
    !,
    \+ private_ip6(A,B,C,D,E,F,G,H).

private_ip4(0,_,_,_).                           % "this" network
private_ip4(10,_,_,_).                          % private
private_ip4(100,B,_,_) :- B >= 64, B =< 127.    % carrier-grade NAT
private_ip4(127,_,_,_).                         % loopback
private_ip4(169,254,_,_).                       % link-local (cloud metadata)
private_ip4(172,B,_,_) :- B >= 16, B =< 31.     % private
private_ip4(192,0,0,_).                         % IETF protocol assignments
private_ip4(192,168,_,_).                       % private
private_ip4(198,B,_,_) :- B >= 18, B =< 19.     % benchmarking
private_ip4(A,_,_,_) :- A >= 224.               % multicast and reserved

private_ip6(0,0,0,0,0,0,0,_).                   % :: and ::1
private_ip6(0,0,0,0,0,0xffff,G,H) :-            % IPv4 mapped
    A is G >> 8, B is G /\ 0xff,
    C is H >> 8, D is H /\ 0xff,
    private_ip4(A,B,C,D).
private_ip6(0x64,0xff9b,_,_,_,_,_,_).           % NAT64
private_ip6(A,_,_,_,_,_,_,_) :- A /\ 0xfe00 =:= 0xfc00.  % unique local
private_ip6(A,_,_,_,_,_,_,_) :- A /\ 0xffc0 =:= 0xfe80.  % link-local
private_ip6(A,_,_,_,_,_,_,_) :- A /\ 0xff00 =:= 0xff00.  % multicast
