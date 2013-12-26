% Debug file for project `plweb`.

% Print code strings with their code table replacements.
:- use_module(library(portray_text)).
:- portray_text(true).
:- set_portray_text(ellipsis, 100).

% Enforce more stringent style checking.
:- style_check(+atom).
:- style_check(+charset).
:- style_check(+discontiguous).
:- style_check(+no_effect).
:- style_check(+singleton).
%:- style_check(+var_branches).

% Notice this gives information to hackers
%  when loaded from within a production environment!
:- use_module(library(http/http_error)).

% This library allows for exploiting the color and attribute facilities
% of most modern terminals using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color) codes.
:- use_module(library(ansi_term)).

% PCE-based debug monitor.
%:- use_module(library(swi_ide)).
%:- prolog_ide(debug_monitor).

% Make some debug message categories visible.
:- use_module(library(debug)).
:- debug(plweb).

% Run unit tests, unless compiled with optimisation turned on.
:- use_module(library(plunit)).
:- set_test_options([load(normal),run(manual)]).
%:- set_test_options([load(normal),run(make(all))]).

:- [load].
:- use_module(news).
:- server.

