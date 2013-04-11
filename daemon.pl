% Avoid that XPCE creates a thread because that stops us forking.

:- set_prolog_flag(xpce_threaded, false).
:- load_files([load], [silent(true)]).
:- initialization http_daemon.

