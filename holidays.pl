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

:- use_module(library(julian)).

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
	Now - Time < 3600.0.
todays_holiday(Holiday) :-
	todays_holiday_(Holiday),
	get_time(Now),
	retractall(current_holiday(_, _)),
	asserta(current_holiday(Now, Holiday)).
todays_holiday_(april_fools_day) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-3-30, 12:0:0]),
	form_time([after(now), Y-4-2, 12:0:0]).
todays_holiday_(christmas) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-12-20, 12:0:0]),
	form_time([after(now), Y-12-27, 12:0:0]).
todays_holiday_(koningsdag) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-4-26, 12:0:0]),
	form_time([after(now), Y-4-28, 12:0:0]).
todays_holiday_(santiklaas) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-12-5, 12:0:0]),
	form_time([after(now), Y-12-6, 12:0:0]).
todays_holiday_(carnival) :-
	carnival_date(Start, End),
	form_time([before(now), Start]),
	form_time([after(now), End]).
todays_holiday_(halloween) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-10-30, 12:0:0]),
	form_time([after(now), Y-11-1, 12:0:0]).
todays_holiday_(none).

carnival_date(2015-2-15, 2015-2-17).
carnival_date(2016-2-7, 2016-2-9).
carnival_date(2017-2-26, 2017-2-28).
carnival_date(2018-2-11, 2018-2-13).
carnival_date(2019-3-3, 2019-3-5).
carnival_date(2020-2-23, 2020-2-25).
carnival_date(2021-2-14, 2021-2-16).
carnival_date(2022-2-27, 2022-2-29).
carnival_date(2023-2-19, 2023-2-21).
carnival_date(2024-2-11, 2025-2-13).
carnival_date(2025-3-2, 2025-3-4).
