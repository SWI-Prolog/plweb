:- module(logstat,
	  [ clean_log/0,
	    read_log/1,			% +File
	    logrecord/1,		% +List
	    logrecord/10
	  ]).
:- use_module(library(rbtrees)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(zlib)).

:- portray_text(true).

/** <module> Process SWI-Prolog HTTPD logfiles

Interesting fields:

	* TimeStamp
	* Session
	* Remote IP
	* Path
	* Query parms
	* Referrer
	* Result+Time

Database:

	logrecord(N, Time, Session, RemoteIP, Path,
		  Query, Referrer, Code, Result, Extra).

Extra fields:

	* cpu(Seconds)
	* user_agent(Agent)
	* http_version(Major-Minor)
	* bytes(Count)

---++ Query API

The main query API is formed by logrecord/1.
*/

:- dynamic
	logrecord/10.

%%	field(?Index, ?Name) is nondet.
%
%	Define the field-names for the logrecord/10 predicate.

field(1, key).
field(2, time).
field(3, session).
field(4, ip).
field(5, path).
field(6, query).
field(7, referer).
field(8, code).
field(9, result).
field(10, extra).


%%	clean_log
%
%	Cleanup the database.

clean_log :-
	functor(Term, logrecord, 10),
	assertion(predicate_property(Term, dynamic)),
	retractall(Term).


%%	read_log(+File)

read_log(File) :-
	myopen(File, In),
	read(In, Term0),
	rb_empty(Open),
	call_cleanup(read_log(Term0, In, 1, Open),
		     close(In)),
	nl(user_error).

myopen(File, In) :-
	exists_file(File), !,
	open(File, read, In, [encoding(utf8)]).
myopen(File, In) :-
	file_name_extension(File, gz, ZFile),
	exists_file(ZFile),!,
	gzopen(ZFile, read, In, [encoding(utf8)]).
myopen(File, In) :-			% generate error
	open(File, read, In, [encoding(utf8)]).


read_log(end_of_file, _, _, _) :- !.
read_log(Term, In, Count0, Open0) :-
	assert_log(Term, Count0, Count1, Open0, Open1), !,
	progress(Count1),
	read(In, Term2),
	read_log(Term2, In, Count1, Open1).
read_log(Term, In, Count, Open) :-
	format(user_error, '~NWarning: failed to process ~p~n', [Term]),
%	gtrace, ignore(assert_log(Term, Count, _Count, Open, _Open)),
	read(In, Term2),
	read_log(Term2, In, Count, Open).

assert_log(server(_StartStop, _Time), Count, Count, Open0, Open) :- !,
	close_all(Open0),
	rb_empty(Open).
assert_log(request(I, Time, Request), Count0, Count, Open0, Open) :- !,
	Count is Count0+1,
	rb_insert_new(Open0, I, r(Count0, Time, Request), Open).
assert_log(completed(I, CPU, Status), Count, Count, Open0, Open) :-
	rb_delete(Open0, I, r(Id, Time, Request), Open),
	save_record(Id, Time, CPU, Request, 0, 0, Status).
assert_log(completed(I, CPU, Bytes, Code, Status), Count, Count, Open0, Open) :-
	rb_delete(Open0, I, r(Id, Time, Request), Open),
	save_record(Id, Time, CPU, Request, Bytes, Code, Status).

close_all(Open0) :-
	rb_visit(Open0, Pairs),
	close_pairs(Pairs).

close_pairs([]).
close_pairs([_RID-r(Id, Time, Request)|T]) :-
	save_record(Id, Time, 0, Request, 0, 500, no_reply),
	close_pairs(T).

save_record(Id, Time, CPU, Request, Bytes, Code, Status) :-
	session(Request, Session),
	remote_IP(Request, RemoteIP),
	path(Request, Path),
	query_parms(Request, Parms),
	referer(Request, Referer),
	extra(Request, Bytes, Extra),
	assert(logrecord(Id, Time, Session, RemoteIP,
			 Path, Parms, Referer, Code, Status,
			 [ cpu(CPU)
			 | Extra
			 ])).


session(Request, SessionID) :-
	memberchk(session(SessionID), Request), !.
session(Request, SessionID) :-
	memberchk(cookie(Cookie), Request),
	memberchk(swipl_session=SessionID, Cookie), !.
session(_, -).

%%	remote_IP(+Request, -IP:atom) is semidet.
%
%	Find the remote IP address. This is either in x_forwarded_for if
%	we are behind a (Apache) reverse proxy, or it is the peer.

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

path(Request, Path) :-
	memberchk(path(Path), Request).

query_parms(Request, Parms) :-
	memberchk(search(Parms), Request), !.
query_parms(_, []).

referer(Request, Referer) :-
	memberchk(referer(Referer), Request), !.
referer(_, -).

extra(Request, Bytes, Extra) :-
	findall(E, extra_field(Request, Bytes, E), Extra).

extra_field(_, Bytes, Extra) :-
	Bytes \== 0,
	Extra = bytes(Bytes).
extra_field(Request, _, user_agent(Agent)) :-
	memberchk(user_agent(Agent), Request).
extra_field(Request, _, http_version(Agent)) :-
	memberchk(http_version(Agent), Request).

%%	logrecord(+Query) is nondet.
%
%	Query by list of fieldnames. Query   list  a list of Name(Value)
%	specifications. Name can be a name as defined by field/2, a Name
%	that appears in the  `Extra'  field,   or  one  of the following
%	defined special fields:
%
%	    * after(+TimeSpec)
%	    Only consider records created after TimeSpec.  TimeSpec is
%	    one of:
%
%	        * Year/Month/Day
%
%	    * before(+TimeSpec)
%	    See after(TimeSpec).
%
%	    * search([Name=Value...])
%	    Demand the following fields to be present in the query.

logrecord(Query) :-
	must_be(list, Query),
	functor(Term, logrecord, 10),
	make_query(Query, Term, RestQuery),
	make_condition(RestQuery, Extra, Term, Cond),
	(   Extra == [], Cond == true
	->  call(Term)
	;   Extra == []
	->  call((Term,Cond))
	;   field(I, extra)
	->  arg(I, Term, ExtraDB),
	    call((Term,Cond)),
	    subset(Extra, ExtraDB)
	).

make_query([], _, []).
make_query([Q|T0], Goal, Extra) :-
	Q =.. [Name,Value],
	fill_field(Name, Goal, Value), !,
	make_query(T0, Goal, Extra).
make_query([Q|T0], Goal, [Q|Extra]) :-
	make_query(T0, Goal, Extra).

fill_field(Name, Term, Value) :-
	field(I, Name),
	arg(I, Term, Value).

make_condition([], [], _, true).
make_condition([C0|T0], T, Term, (Goal,More)) :-
	condition(C0, Term, Goal), !,
	make_condition(T0, T, Term, More).
make_condition([H|T0], [H|T], Term, Cond) :-
	make_condition(T0, T, Term, Cond).


condition(after(TimeSpec), Term, Stamp >= Time) :-
	field(I, time), !,
	arg(I, Term, Stamp),
	time_spec_to_stamp(TimeSpec, Time).
condition(before(TimeSpec), Term, Stamp < Time) :-
	field(I, time), !,
	arg(I, Term, Stamp),
	time_spec_to_stamp(TimeSpec, Time).
condition(search(Fields), Term, subset(Fields, Query)) :-
	field(I, query), !,
	arg(I, Term, Query).


time_spec_to_stamp(Y/M/D, Stamp) :-
	date_time_stamp(date(Y,M,D,0,0,0,0,-,-), Stamp).


		 /*******************************
		 *	       FEEDBACK		*
		 *******************************/

progress(Count) :-
	Count mod 1000 =:= 0, !,
	format(user_error, '\r~t~D~20|', [Count]).
progress(_).
