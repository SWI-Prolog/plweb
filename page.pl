/*  File:    page.pl
    Author:  Jan Wielemaker
    Created: Jan  9 2009
    Purpose: Main page layout
*/

:- module(plweb_page,
	  [ sidebar//0
	  ]).
:- use_module(library(http/html_write)).

%%	sidebar//
%
%	Emit the sidebar with logo and menu

sidebar -->
	html([ div(class(logo), img(src('/icons/swipl.jpeg'))),
	       div(class(menu), menu)
	     ]).
