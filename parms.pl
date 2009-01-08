/*  File:    parms.pl
    Author:  Jan Wielemaker
    Created: Jan  8 2009
    Purpose: Parameters for plweb
*/

:- module(plweb_parms,
	  [ 
	  ]).
:- use_module(library(http/http_path)).
:- use_module(library(settings)).


:- setting(http:served_file_extensions,
	   list(atom),
	   [ html ],
	   'List of extensions that are served as plain files').
:- setting(http:port,
	   integer,
	   3040,
	   'Default port').


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
