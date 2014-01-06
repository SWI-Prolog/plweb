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
	  [ location_wiki_file/2
	  ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(git)).
:- use_module(git_html).
:- use_module(markitup).
:- use_module(notify).
:- use_module(openid).

/** <module> Edit PlDoc wiki pages


*/

:- http_handler(root(wiki_edit),    wiki_edit, []).
:- http_handler(root(wiki_save),    wiki_save, []).
:- http_handler(root(wiki/sandbox), wiki_sandbox, []).

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
	authenticate(Request, Fields),
	nth1(2, Fields, Author),
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
			\edit_page(Location, File, Author)).

edit_page(Location, File, Author) -->
	{ (   exists_file(File)
	  ->  read_file_to_codes(File, Codes, []),
	      string_to_list(Content, Codes),
	      Title = 'Edit',
	      file_directory_name(File, Dir)
	  ;   Content = '',
	      Title = 'Create',
	      Dir = _			% shortlog//2 is quiet on var
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
					\markitup([ markup(pldoc),
						    id(text),
						    value(Content)
						  ]))),
				  tr([td(class(label), 'Comment summary:'),
				      td(input([class(git_msg), name(msg)]))]),
				  tr([td(class(label), 'Comment:'),
				      td(textarea([ class(git_comment), cols(55), rows(5), name(comment)],
						  ''))]),
				  tr(td([ align(right), colspan(2) ],
					[ \amend_button(Dir, File, Author), ' ',
					  input([type(submit), value(save)])
					]))
				])
			])
		 ])).

%%	amend_button(+Dir, +File, +Author)//
%
%	Show button to amend the previous commit.

amend_button(Dir, File, Author) -->
	{ exists_file(File),
	  git_shortlog(Dir, [ShortLog], [path(File), limit(1)]),
	  git_log_data(author_name, ShortLog, LastAuthor),
	  debug(git, 'Amend: LastAuthor = ~q, Author = ~q', [LastAuthor, Author]),
	  LastAuthor == Author
	},
	html([ input([class(amend),
		      type(checkbox), name(amend), value(yes)]),
	       'Amend previous commit'
	     ]).
amend_button(_,_,_) --> [].

%%	shortlog(+Dir, +Options)//
%
%	Include a GIT shortlog

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
			  amend(Amend,
				[ optional(true),
				  description('Amend previous commit')
				]),
			  msg(Msg, []),
			  comment(Comment, [optional(true)])
			]),
	location_wiki_file(Location, File),
	allowed_file(File),
	(   exists_file(File)
	->  New = false
	;   New = true
	),
	save_file(File, Text),
	(   var(Comment)
	->  GitMsg = Msg
	;   atomic_list_concat([Msg, Comment], '\n\n', GitMsg)
	),
	file_directory_name(File, Dir),
	file_base_name(File, Rel),
	(   New == true
	->  git([add, Rel], [ directory(Dir) ])
	;   true
	),
	atom_concat('--author=', Author, AuthorArg),
	GitArgs0 = [ '-m', GitMsg, AuthorArg, Rel ],
	(   Amend == yes
	->  append([commit, '--amend'], GitArgs0, GitArgs)
	;   append([commit], GitArgs0, GitArgs)
	),
	git(GitArgs,
	    [ directory(Dir)
	    ]),
	notify(wiki(Location), wiki_edit(Text)),
	http_redirect(see_other, Location, Request).

author([_User, Name, EMail], Author) :- !,
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
			   write_text(Out, Text),
			   close(Out)).

%%	write_text(+Out, +Text:atom) is det.
%
%	Write the text. Text may have  LF   or  CR/LF line endings. This
%	code fixes this. I'm not sure  output encoding issues. Hopefully
%	the text is submitted as UTF-8 and converted appropriately.

write_text(Out, Text) :-
	forall(sub_atom(Text, _, 1, _, Char),
	       put_non_cr(Out, Char)).

put_non_cr(_Out, Char) :-
	char_code(Char, 13), !.
put_non_cr(Out, Char) :-
	put_char(Out, Char).


%%	authenticate(+Request, -Fields)
%
%	Get authentication for editing wiki pages.  This now first tries
%	the OpenID login.

authenticate(Request, Fields) :-
	authenticate(Request, wiki, Fields).


%%	allowed_file(+File) is det.
%
%	@error	permission_error(edit, file, File) if the user is not
%		allowed to edit File.

allowed_file(File) :-
	absolute_file_name(document_root(.),
			   DocRoot,
			   [ file_type(directory)
			   ]),
	sub_atom(File, 0, _, _, DocRoot),
	access_file(File, write), !.
allowed_file(File) :-
	permission_error(edit, file, File).


hidden(Name, Value) -->
	html(input([type(hidden), name(Name), value(Value)])).


%%	wiki_sandbox(+Request)
%
%	HTTP handler that displays a Wiki sandbox

wiki_sandbox(_Request) :-
	reply_html_page(wiki(sandbox),
			title('PlDoc wiki sandbox'),
			[ \sandbox
			]).

sandbox -->
	{ http_absolute_location(root('pldoc/package/pldoc.html'), PlDoc, [])
	},
	html([ p([ 'This page provides a sandbox for the ',
		   a(href(PlDoc), 'PlDoc'),
		   ' wiki format.  The preview window is updated every ',
		   'time you hit the RETURN or TAB key.'
		 ]),
	       p([ 'Note that PlDoc wiki is normally embedded in a ',
		   'Prolog source file using a ', i('structured comment'),
		   ', i.e., a comment that starts with %! or /**'
		 ]),
	       div(\markitup([ markup(pldoc),
			       preview(true)
			     ]))
	     ]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	mail_notify:event_subject//1,		% +Event
	mail_notify:event_message//1.		% +event

mail_notify:event_subject(wiki_edit(_)) -->
	[ 'Wiki edit'-[] ].

mail_notify:event_message(wiki_edit(Text)) -->
	[ 'Wiki edit'-[],
	  nl, nl,
	  '====~n~w~n===='-[Text],
	  nl
	].
