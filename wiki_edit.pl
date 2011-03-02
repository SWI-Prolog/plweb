/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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

:- module(wiki_edit,
	  [
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_authenticate)).
:- use_module(git).
:- use_module(git_html).


/** <module> Edit PlDoc wiki pages


*/

:- http_handler(root(wiki_edit), wiki_edit, []).
:- http_handler(root(wiki_save), wiki_save, []).

%%	edit_button(+Location)//
%
%	Present a button for editing the web-page

:- public edit_button//1.
:- multifile edit_button//1.

edit_button(Location) -->
	{ http_link_to_id(wiki_edit, [location(Location)], HREF) },
	html(a(href(HREF),
	       img([ class(action),
		     alt(edit),
		     title('Edit wiki page'),
		     src(location_by_id(pldoc_resource)+'edit.gif')
		   ]))).


		 /*******************************
		 *	       SHOW		*
		 *******************************/

%%	wiki_edit(+Request)
%
%	HTTP handler that deals with editing a wiki page.

wiki_edit(Request) :-
	authenticate(Request, _Fields),
	http_parameters(Request,
			[ location(Location,
				   [ description('Wiki location to edit')
				   ])
			]),
	location_wiki_file(Location, File),
	allowed_file(File),
	file_base_name(File, BaseName),
	reply_html_page(wiki,
			title('Edit ~w'-[BaseName]),
			\edit_page(Location, File)).

edit_page(Location, File) -->
	{ (   exists_file(File)
	  ->  read_file_to_codes(File, Codes, []),
	      string_to_list(Content, Codes),
	      Title = 'Edit',
	      file_directory_name(File, Dir)
	  ;   Content = '',
	      Title = 'Create'
	  ),
	  http_location_by_id(wiki_save, Action)
	},
	html(div(class(wiki_edit),
		 [ h1(class(wiki), [Title, ' ', Location]),
		   \shortlog(Dir, [path(File), limit(5)]),
		   form(action(Action),
			[ \hidden(location, Location),
			  table(class(wiki_edit),
				[ tr(td([ class(wiki_text), colspan(2) ],
					textarea([ cols(80),rows(20),name(text) ],
						 Content))),
				  tr([td(class(label), 'Comment summary:'),
				      td(input([class(git_msg), name(msg)]))]),
				  tr([td(class(label), 'Comment:'),
				      td(textarea([ class(git_comment), cols(55), rows(5), name(comment)],
						  ''))]),
				  tr(td([ align(right), colspan(2) ],
					input([type(submit), value(save)])))
				])
			])
		 ])).


shortlog(Dir, _Options) -->
	{ var(Dir) }, !.
shortlog(Dir, Options) -->
	html_requires(css('git.css')),
	git_shortlog(Dir, Options).


		 /*******************************
		 *	       SAVE		*
		 *******************************/

%%	wiki_save(+Request)
%
%	HTTP handler that saves a new or modified wiki page.

wiki_save(Request) :-
	authenticate(Request, Fields),
	author(Fields, Author),
	http_parameters(Request,
			[ location(Location,
				   [ description('Path of the file to edit')
				   ]),
			  text(Text,
			       [ description('Wiki content for the file')
			       ]),
			  msg(Msg, []),
			  comment(Comment, [optional(true)])
			]),
	location_wiki_file(Location, File),
	allowed_file(File),
	(   exists_file(File)
	->  New = true
	;   New = false
	),
	save_file(File, Text),
	(   var(Comment)
	->  GitMsg = Msg
	;   atomic_list_concat([Msg, Comment], '\n\n', GitMsg)
	),
	file_directory_name(File, Dir),
	(   New == true
	->  git([add, File], [ directory(Dir) ])
	;   true
	),
	git([commit,
	     '-m', GitMsg,
	     '--author', Author,
	     File
	    ],
	    [ directory(Dir)
	    ]),
	http_redirect(see_other, Location, Request).

author([_User, Name, EMail], Author) :-
	atomic_list_concat([Name, ' <', EMail, '>'], Author).
author([_User, Name], Author) :-
	atomic_list_concat([Name, ' <nospam@nospam.org>'], Author).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	location_wiki_file(+Location, -Path)
%
%	@see Merge with find_file from plweb.pl

location_wiki_file(Relative, File) :-
	file_name_extension(Base, html, Relative),
	file_name_extension(Base, txt, WikiFile),
	absolute_file_name(document_root(WikiFile),
			   File,
			   [ access(write),
			     file_errors(fail)
			   ]), !.
location_wiki_file(Relative, File) :-
	file_name_extension(_, txt, Relative),
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(write),
			     file_errors(fail)
			   ]).
location_wiki_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   Dir,
			   [ file_type(directory),
			     file_errors(fail)
			   ]),
	setting(http:index_files, Indices),
        member(Index, Indices),
	directory_file_path(Dir, Index, File),
        access_file(File, write), !.

%%	save_file(+File, +Text)
%
%	Modify the file.

save_file(File, Text) :-
	setup_call_cleanup(open(File, write, Out,
				[ encoding(utf8)
				]),
			   write(Out, Text),
			   close(Out)).


%%	authenticate(+Request, -Fields)
%
%

authenticate(Request, Fields) :-
	(   http_authenticate(basic(passwd), Request, Fields)
	->  true
	;   throw(http_reply(authorise(basic, 'SWI-Prolog wiki editor')))
	).

%%	allowed_file(+File) is semidet.

allowed_file(_).

hidden(Name, Value) -->
	html(input([type(hidden), name(Name), value(Value)])).
