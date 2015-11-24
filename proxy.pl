/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(plweb_proxy,
	  [ proxy/2			% +Target, +Request
	  ]).

:- use_module(library(http/http_open)).

%%	proxy(+To, +Request) is det.
%
%	Proxy a request to a remote   server.  This proxies all methods,
%	including those carrying data such as POST and PUT.

proxy(To, Request) :-
	memberchk(method(Method), Request),
	proxy(Method, To, Request).

proxy(Method, To, Request) :-
	data_method(Method), !,
	read_data(Request, Data),
	memberchk(request_uri(URI), Request),
        atomic_list_concat([To,URI], Target),
	http_open(Target, In,
		  [ method(Method),
		    post(Data),
		    header(content_type, ContentType)
		  ]),
        call_cleanup(
	    read_string(In, _, Bytes),
	    close(In)),
	throw(http_reply(bytes(ContentType, Bytes))).
proxy(Method, To, Request) :-
	memberchk(request_uri(URI), Request),
        atomic_list_concat([To,URI], Target),
	http_open(Target, In,
		  [ method(Method),
		    header(content_type, ContentType)
		  ]),
        call_cleanup(
	    read_string(In, _, Bytes),
	    close(In)),
	throw(http_reply(bytes(ContentType, Bytes))).

read_data(Request, bytes(ContentType, Bytes)) :-
	memberchk(input(In), Request),
	memberchk(content_type(ContentType), Request),
	(   memberchk(content_length(Len), Request)
	->  read_string(In, Len, Bytes)
	;   read_string(In, _, Bytes)
	).

data_method(post).
data_method(put).
