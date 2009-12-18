:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(hello_world), hello_world, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

hello_world(_Request) :-
	format('Content-type: text/plain~n~n'),
	format('Hello World!~n').
