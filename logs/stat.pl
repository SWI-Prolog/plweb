:- module(stat,
	  [ stat/1,
	    stat2jpeg/2
	  ]).
:- use_module(library(pce)).
:- use_module(library(autowin)).
:- use_module(library('plot/barchart')).
:- use_module(library(gradient)).
:- use_module(library(lists)).

:- ensure_x_server(125, 24).

stat(Terms) :-
	Win = @stats,
	free(Win),
	stat(Terms, BarChart),
	new(Win, auto_sized_picture('Statistics')),
	send(Win, display, BarChart),
	send(Win, open).

stat2jpeg(Terms, Jpeg) :-
	stat(Terms, BarChart),
	get(@pce, convert, BarChart, pixmap, Img),
	send(Img, save, Jpeg, jpeg).

stat(Terms, BC) :-
	length(Terms, N),
	new(BC, bar_chart(vertical, 0, 15000, 300, N, 15, 2)),
	title(BC, Terms),
	legenda(Legenda),
	send(BC, display, Legenda, point(50, 50)),
	(   member(downloads(Mon, Y, W32, W64, M, L, S), Terms),
	    T is W32+W64+M+L+S,
	    T > 0,
	    Y >= 2000,
	    concat_atom([Mon, -, Y], Label),
	    (	member(date(Y, Mon, Day), Terms)
	    ->  Day > 2,
	        XS is round(S * 30/Day) - S,
		XL is round(L * 30/Day) - L,
		XM is round(M * 30/Day) - M,
		XW32 is round(W32 * 30/Day) - W32,
		XW64 is round(W64 * 30/Day) - W64,
	        send(BC, append,
		     bar_stack(Label,
			       download_bar(source,  true,  XS),
			       download_bar(source,  false,  S),
			       download_bar(linux,   true,  XL),
			       download_bar(linux,   false,  L),
			       download_bar(mac,     true,  XM),
			       download_bar(mac,     false,  M),
			       download_bar(win32,   true,  XW32),
			       download_bar(win32,   false,  W32),
			       download_bar(win64,   true,  XW64),
			       download_bar(win64,   false,  W64)))
	    ;	send(BC, append,
		     bar_stack(Label,
			       download_bar(source,  false, S),
			       download_bar(linux,   false, L),
			       download_bar(mac,     false, M),
			       download_bar(win32,   false, W32),
			       download_bar(win64,   false, W64)))
	    ),
	    fail
	;   send(BC, nbars)
	).

title(BC, Terms) :-
	member(date(Y, Mon, Day), Terms), !,
	send(BC, display,
	     text(string('SWI-Prolog downloads until %d %s %s',
			 Day, Mon, Y),
		  left,
		  boldlarge),
	     point(50, 15)).
title(BC, _) :-
	send(BC, display,
	     text(string('SWI-Prolog downloads'), left, boldlarge),
	     point(50, 15)).


legenda(Dev) :-
	new(Dev, figure),
	send(Dev, format, new(F, format(horizontal, 2, @on))),
	send(F, row_sep, 0),
	legenda_entry(Dev, source,  false, 'Source'),
	legenda_entry(Dev, linux,   false, 'Linux RPM (exluding CD-distributions)'),
	legenda_entry(Dev, mac,     false, 'MacOS X'),
	legenda_entry(Dev, win32,   false, 'MS-Windows'),
	legenda_entry(Dev, win64,   false, 'MS-Windows 64-bit edition'),
	legenda_entry(Dev, win64,   true,  'interpolated (current month)').

legenda_entry(Dev, Id, Invert, Label) :-
	send(Dev, display, new(B, box(15, 10))),
	send(B, fill_offset, point(0,0)),
	bar_colour(Id, Invert, Gradient),
	send(B, fill_pattern, Gradient),
	send(Dev, display, text(Label)).
			
:- pce_begin_class(download_bar, bar).

initialise(Bar, Type:name, Estimate, Value:int) :->
	bar_colour(Type, Estimate, Colour),
	send_super(Bar, initialise, Type, Value, Colour).

:- pce_end_class(download_bar).


		 /*******************************
		 *	      COLOURS		*
		 *******************************/


%	nth_bar_gradient(N, Hue, S, Vtop, VBottom)

bar_gradient(source,   193, 80, 100, 20).
bar_gradient(linux,    300, 80, 100, 20).
bar_gradient(mac,      160, 80, 100, 20).
bar_gradient(win32,     54, 80, 100, 20).
bar_gradient(win64,    340, 80, 100, 20).
bar_gradient( 5,  80, 80, 100, 20).
bar_gradient( 6,  45, 80, 100, 20).
bar_gradient( 7,  61, 80, 100, 20).
bar_gradient( 8, 280, 80, 100, 20).
bar_gradient( 9, 225, 80, 100, 20).

:- dynamic
	colour_cache/2.

bar_colour(Name, Inverse, Img) :-
	bar_gradient(Name, H, S0, VT, VB), !,
	(   Inverse == true
	->  Va = VT, Vz = VB, S = 30
	;   Va = VB, Vz = VT, S = S0
	),
	new(Img, gradient(@nil,
			  colour(@default, H, S, Va, hsv),
			  colour(@default, H, S, Vz, hsv),
			  15, horizontal)),
	asserta(colour_cache(Name, Img)).

