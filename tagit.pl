/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

:- module(tagit,
	  [ tagit//1
	  ]).
:- use_module(library(option)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- html_resource(tagit,
		 [ virtual(true),
		   requires([ jquery_ui,
			      js('tagit/css/jquery.tagit.css'),
			      js('tagit/css/tagit.ui-zendesk.css'),
			      js('jquery-ui/themes/base/jquery.ui.autocomplete.css'),
			      js('tagit/js/tag-it.min.js')
			    ])
		 ]).

%%	tagit(+Options)// is det.
%
%	HTML rule to include a tagit object

tagit(Options) -->
	{ option(id(Id), Options, tags)
	},
	html_requires(tagit),
	html([ input([id(Id)]),
	       script([type('text/javascript')],
		      [ \[ '$(document).ready(function() {\n',
			   '  $("#',Id,'").tagit({\n'
			 ],
		      \tagit_options(Options),
			\[ '    dummy:true\n',
			   '  });\n',
			   '});\n'
			 ]
		      ])
	     ]).

tagit_options([]) --> [].
tagit_options([H|T]) --> tagit_option(H), tagit_options(T).

tagit_option(autocomplete(URL)) --> !,
	html(\['    autocomplete: {delay: 0, minLength: 2, source:"~w"},\n'-[URL]]).
tagit_option(remove_confirmation(true)) --> !,
	html(\['    removeConfirmation: true,\n']).
tagit_option(on_click(URL)) --> !,
	html(\['    onTagClicked: function(event, ui) {\n',
	       '      $.ajax({ url:"~w?tag="+encodeURIComponent(ui.tagLabel) });\n'-[URL],
	       '    },\n'
	      ]).
tagit_option(_) --> [].

