:- module(http_limit,
	  [
	  ]).
:- use_module(library(broadcast)).
:- use_module(library(lists)).

/** <module> Limit HTTP requests from the same place

This module exploits the broadcasting  service   to  allow only a single
concurrent download of the same location from the same IP. It does so by
tracking active connections based on the broadcast messages and throw an
HTTP busy if there is already an   ongoing connection for the given path
from the provided IP.

Somehow, some requests are not terminated.  We maintain a 15-minute bann
and assume the active listing is bogus afterwards.
*/

:- listen(http(Message),
	  http_message(Message)).

http_message(request_start(Id, Request)) :- !,
	start(Id, Request).
http_message(request_finished(Id, _Code, _Status, _CPU, _Bytes)) :- !,
	end(Id).

:- dynamic
	active/4.			% Id, Path, IP, Time

%%	start(+Id, +Request) is det.
%
%	If a connection is active, say we  are busy. Note that this code
%	does no locking. That implies there is   a  small change you get
%	away inserting two requests at the same   time, but it is pretty
%	unlikely and not harmful, so we ignore that.
%
%	@throws http_reply(busy) if a connection is active

start(Id, Request) :-
	remote_IP(Request, IP),
	memberchk(path(Path), Request),
	memberchk(range(_), Request),	% only limit range-requests
	get_time(Now),
	(   active(Id2, Path, IP, Since),
	    (	Now-Since < 900		% 15 minutes bann
	    ->	true
	    ;	retractall(active(Id2, _, _, _)),
		fail
	    )
	->  throw(http_reply(busy))
	;   asserta(active(Id, Path, IP, Now))
	).

end(Id) :-
	retractall(active(Id, _, _, _)).

remote_IP(Request, IP) :-
        memberchk(x_forwarded_for(IP0), Request), !,
        final_ip(IP0, IP).
remote_IP(Request, IP) :-
        memberchk(peer(Peer), Request), !,
        peer_to_ip(Peer, IP).
remote_IP(_, -).

final_ip(IP0, IP) :-
        sub_atom(IP0, _,_,_, ', '), !,
        findall(A, sub_atom(IP0, _, _, A, ', '), As),
        last(As, A),
        sub_atom(IP0, _,A,0, IP).
final_ip(IP, IP).

peer_to_ip(ip(A,B,C,D), IP) :-
        atomic_list_concat([A,B,C,D], '.', IP).
