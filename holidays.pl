/*  Part of SWI-Prolog

    Author:        Anne Ogborn
    WWW:           http://www.swi-prolog.org

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
:- module(holidays, [
	      todays_holiday/1
	  ]).

:- dynamic current_holiday/2.

current_holiday(0, none).

/**  todays_holiday(-Holiday:atom) is det
 *
 *  succeeds if Holiday is 'todays holiday'
 *
 *  This is none on 'ordinary' days, or
 *  one of
 *
 *  april_fools_day
 *  christmas
 *  koningsdag
 *  santiklaas
 *
 * succeeds only within 12 hours either
 * side of April 1
 *
 * April fools day is a traditional holiday celebrated
 * by playing hoaxes and practical jokes. A common form
 * of this is to substitute nonsensical information where
 * useful info is normally displayed.
 *
 * Koningsdag is 'Kings day' in the Netherlands
 * Santiklaas is St. Nicholas' feast
 *
 */
todays_holiday(Holiday) :-
	current_holiday(Time, Holiday),
	get_time(Now),
	Now - Time < 3600, !.
todays_holiday(Holiday) :-
	year(Year),
	todays_holiday(Year, Holiday),
	get_time(Now),
	retractall(current_holiday(_, _)),
	asserta(current_holiday(Now, Holiday)).

todays_holiday(YY, april_fools_day) :-
	date_between(YY-03-30, 12:00:00,
		     YY-04-02, 12:00:00).
todays_holiday(YY, christmas) :-
	date_between(YY-12-20, 12:00:00,
		     YY-12-27, 12:00:00).
todays_holiday(YY, koningsdag) :-
	date_between(YY-04-26, 12:00:00,
		     YY-04-28, 12:00:00).
todays_holiday(YY, santiklaas) :-
	date_between(YY-12-05, 12:00:00,
		     YY-12-06, 12:00:00).
todays_holiday(YY, halloween) :-
	date_between(YY-10-30, 12:00:00,
		     YY-11-01, 12:00:00).
todays_holiday(YY, carnival) :-
	carnival_date(Start, End),
	Start = (YY-_-_),
	End   = (YY-_-_),
	date_between(Start, 12:00:00,
		     End,   12:00:00).
todays_holiday(_, none).

carnival_date(2015-2-15, 2015-2-17).
carnival_date(2016-2-07, 2016-2-09).
carnival_date(2017-2-26, 2017-2-28).
carnival_date(2018-2-11, 2018-2-13).
carnival_date(2019-3-03, 2019-3-05).
carnival_date(2020-2-23, 2020-2-25).
carnival_date(2021-2-14, 2021-2-16).
carnival_date(2022-2-27, 2022-2-29).
carnival_date(2023-2-19, 2023-2-21).
carnival_date(2024-2-11, 2025-2-13).
carnival_date(2025-3-02, 2025-3-04).

%%	year(-Year)
%
%	True when Year is the current year

year(Year) :-
	get_time(Now),
	stamp_date_time(Now, Term, 'UTC'),
	date_time_value(year, Term, Year).

date_between(SDate, STime, EDate, ETime) :-
	get_time(Now),
	stamp(SDate, STime, Start),  Now >= Start,
	stamp(EDate, ETime, End),    Now =< End.

stamp(YY-MM-DD, H:M:S, Time) :-
	date_time_stamp(date(YY,MM,DD,H,M,S,0,'UTC',-), Time).
