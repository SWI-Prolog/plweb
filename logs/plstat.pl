:- module(plstat,
	  [ downloads/3			% -Term
	  ]).
:- use_module(logstat).
:- use_module(library(aggregate)).

% downloads(Month, Year, Counts:pairs)

downloads(Year, Month, Pairs) :-
	setof(Platform=Count,
	      aggregate(count, download(Year, Month, Platform), Count),
	      Pairs).


download(Year, Month, Platform) :-
	(   logrecord([time(Stamp), path(Path), code(200)])
	*-> true
	;   logrecord([time(Stamp), path(Path), result(file(_))])
	),
	atom_concat('/download/', More, Path),
	atom_codes(More, Codes),
	phrase(file_details(_Version, Platform), Codes),
	stamp_date_time(Stamp, Date, 'UTC'),
	date_time_value(year, Date, Year),
	date_time_value(month, Date, Month).

file_details(Version, Platform) -->
	string(_), "/", !,
	file_details(Version, Platform).
file_details(Version, win32) -->
	"w32pl",
	short_version(Version),
	".exe".
file_details(Version, win64) -->
	"w64pl",
	short_version(Version),
	".exe".
file_details(Version, linux) -->
	"pl-",
	long_version(Version),
	"-", integer(_), ".", identifier(_), ".rpm".
file_details(Version, source) -->
	"pl-",
	long_version(Version),
	".tar.gz".
file_details(Version, doc) -->
	"pl-doc-",
	long_version(Version),
	".tar.gz".
file_details(Version, macos) -->
	"swipl-",
	long_version(Version),
	"-mac",
	skip_rest(_).
file_details(Version, macos) -->
	"swi-prolog-",
	long_version(Version),
	mac.
file_details(Version, macos) -->
	"swi-prolog-devel-",
	long_version(Version),
	mac.

mac --> "-mac", !, skip_rest(_).
mac --> ".mpkg", !, skip_rest(_).
mac --> "-tiger.mpkg", !, skip_rest(_).
mac --> "-tiger-intel.mpkg", !, skip_rest(_).

integer(I) -->
	digit(D0),
	digits(DT),
	{ number_codes(I, [D0|DT])
	}.

digit(D) -->
	[D],
	{ code_type(D, digit)
	}.

digits([H|T]) -->
	digit(H), !,
	digits(T).
digits([]) -->
	[].

identifier(Atom) -->
	[C0],
	{ code_type(C0, alpha)
	},
	alnums(CT),
	{ atom_codes(Atom, [C0|CT])
	}.

alnums([H|T]) -->
	[H],
	{ code_type(H, alnum)
	}, !,
	alnums(T).
alnums([]) -->
	[].

string([]) -->
	[].
string([H|T]) -->
	[H],
	string(T).

skip_rest(Rest, Rest, "").

long_version(V) -->
	integer(Major), ".", integer(Minor), ".", integer(Patch),
	{ concat_atom([Major, Minor, Patch], '.', V)
	}.

short_version(V) -->
	digit(Major),
	digit(Minor),
	digits(Patch),
	{ atom_codes(V, [Major, 0'., Minor, 0'. | Patch])
	}.
