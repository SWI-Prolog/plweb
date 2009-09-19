:- module(stat,
	  [ stat/1,
	    stat2jpeg/2			% +Out, +Terms
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

stat2jpeg(Jpeg, Terms) :-
	stat(Terms, BarChart),
	get(@pce, convert, BarChart, pixmap, Img),
	send(Img, save, Jpeg, jpeg).

stat(Terms, BC) :-
	length(Terms, N),
	new(BC, bar_chart(vertical, 0, 15000, 300, N, 15, 2)),
	title(BC, Terms),
	legenda(Legenda),
	send(BC, display, Legenda, point(50, 50)),
	(   member(downloads(Y, Mon, Counts), Terms),
	    sum_counts(Counts, T),
	    T > 0,
	    Y >= 2000,
	    count(source, Counts, S),
	    count(linux,  Counts, L),
	    count(mac,    Counts, M),
	    count(win32,  Counts, W32),
	    count(win64,  Counts, W64),
	    count(doc,    Counts, DOC),
	    month_name(Mon, Month),
	    atomic_list_concat([Month, -, Y], Label),
	    (	member(date(Y, Mon, Day), Terms)
	    ->  Day > 2,
	        XS is round(S * 30/Day) - S,
		XL is round(L * 30/Day) - L,
		XM is round(M * 30/Day) - M,
		XW32 is round(W32 * 30/Day) - W32,
		XW64 is round(W64 * 30/Day) - W64,
		XDOC is round(DOC * 30/Day) - DOC,
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
			       download_bar(win64,   false,  W64),
			       download_bar(doc,     true, XDOC),
			       download_bar(doc,     false, XDOC)))
	    ;	send(BC, append,
		     bar_stack(Label,
			       download_bar(source,  false, S),
			       download_bar(linux,   false, L),
			       download_bar(mac,     false, M),
			       download_bar(win32,   false, W32),
			       download_bar(win64,   false, W64),
			       download_bar(doc,     false, DOC)))
	    ),
	    fail
	;   send(BC, nbars)
	).

sum_counts([], 0).
sum_counts([_=C|T], Sum) :-
	sum_counts(T, Sum0),
	Sum is C+Sum0.

count(Which, Counts, Count) :-
	memberchk(Which=Count, Counts), !.
count(_, _, 0).

month_name(1, jan).
month_name(2, feb).
month_name(3, mar).
month_name(4, apr).
month_name(5, may).
month_name(6, jun).
month_name(7, jul).
month_name(8, aug).
month_name(9, sep).
month_name(10, oct).
month_name(11, nov).
month_name(12, dec).

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
	legenda_entry(Dev, doc,     false, 'Documentation'),
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
bar_gradient(doc,      250, 80, 100, 20).
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

