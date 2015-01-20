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
todays_holiday(april_fools_day) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-3-30, 12:0:0]),
	form_time([after(now), Y-4-2, 12:0:0]).
todays_holiday(christmas) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-12-20, 12:0:0]),
	form_time([after(now), Y-12-27, 12:0:0]).
todays_holiday(koningsdag) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-4-26, 12:0:0]),
	form_time([after(now), Y-4-28, 12:0:0]).
todays_holiday(santiklaas) :-
	form_time([now, Y-_-_]),
	form_time([before(now), Y-12-5, 12:0:0]),
	form_time([after(now), Y-12-6, 12:0:0]).
todays_holiday(none).

