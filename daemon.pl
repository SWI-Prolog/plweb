:- use_module(library(http/http_log)).

% Avoid that XPCE creates a thread because that stops us forking.
:- set_prolog_flag(xpce_threaded, false).
:- load_files([load], [silent(true)]).

:- http_schedule_logrotate(monthly(1, 04:00),
			   [ keep_logs(6)
			   ]).

:- initialization http_daemon.

