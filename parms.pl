/*  File:    parms.pl
    Author:  Jan Wielemaker
    Created: Jan  8 2009
    Purpose: Parameters for plweb
*/

:- module(plweb_parms,
	  [ 
	  ]).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(settings)).


:- setting(http:served_file_extensions,
	   list(atom),
	   [ html, gif, png, jpeg, css, js ],
	   'List of extensions that are served as plain files').
:- setting(http:index_files,
	   list(atom),
	   [ 'index.txt', 'index.html' ],
	   'List of files that provide a directory index').
:- setting(http:port,
	   integer,
	   3040,
	   'Default port').


		 /*******************************
		 *	     LOCATIONS		*
		 *******************************/

http:location(pldoc, root(pldoc), [priority(10)]).
http:location(pldoc_resource, Path, []) :-
	http_location_by_id(pldoc_resource, Path).
http:location(download, root(download), []).
http:location(icons,    root(icons), []).

		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

:- html_resource(plweb,
		 [ virtual(true),
		   requires([ pldoc_resource('pldoc.css'),
			      root('css/plweb.css')
			    ])
		 ]).



		 /*******************************
		 *	       FILES		*
		 *******************************/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   (   user:file_search_path(plweb, Dir)
   ->  true
   ;   asserta(user:file_search_path(plweb, Dir))
   ).

user:file_search_path(document_root, plweb(www)).
user:file_search_path(download,	     plweb(download)).
