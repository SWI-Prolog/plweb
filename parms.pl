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
:- use_module(library(www_browser)).
:- use_module(library(settings)).


:- setting(http:served_file_extensions,
	   list(atom),
	   [ html, gif, png, jpeg, jpg, css, js, tgz, exe, c, zip ],
	   'List of extensions that are served as plain files').
:- setting(http:index_files,
	   list(atom),
	   [ 'index.txt', 'index.html' ],
	   'List of files that provide a directory index').
:- setting(http:port,
	   integer,
	   3040,
	   'Default port').
:- setting(http:workers,
	   integer,
	   5,
	   'Number of worker threads').


		 /*******************************
		 *	     LOCATIONS		*
		 *******************************/

http:location(pldoc,	root(pldoc),    [priority(10)]).
http:location(download, root(download), []).
http:location(icons,    root(icons),    []).
http:location(css,	root(css),      []).


		 /*******************************
		 *	   EXTERNAL URLS	*
		 *******************************/

:- multifile
	user:url_path/2.

user:url_path(swi,	'http://www.swi-prolog.org').
user:url_path(hcs,	'http://hcs.science.uva.nl').
user:url_path(pkg,	swi(packages)).
user:url_path(dmoz,	'http://dmoz.org').
user:url_path(dmoz_pl,	dmoz('Computers/Programming/Languages/Prolog/Implementations/')).
user:url_path(ffii, 	'http://swpat.ffii.org/index.en.html').
user:url_path(fsf, 	'http://www.fsf.org').
user:url_path(gpl, 	fsf('copyleft/gpl.html')).
user:url_path(lgpl, 	fsf('copyleft/lesser.html')).
user:url_path(wordnet, 	'http://www.cogsci.princeton.edu/~wn/').
user:url_path(gmp,	'http://www.swox.com/gmp/').
user:url_path(mailman,	'http://www.list.org/').
user:url_path(bonn,	'https://mailbox.iai.uni-bonn.de/mailman').
user:url_path(maillist,	bonn('listinfo.cgi/swi-prolog')).
user:url_path(mailarchive, bonn('public/swi-prolog/')).
user:url_path(oldmail,	'http://gollem.science.uva.nl/SWI-Prolog/mailinglist/archive/').
user:url_path(nabble,	'http://www.nabble.com').
user:url_path(pl_nabble, nabble('SWI-Prolog-f448.html')).
user:url_path(gmane,     'http://blog.gmane.org').
user:url_path(pl_gmane,  gmane('gmane.comp.ai.prolog.swi')).
user:url_path(chr_mail,  'http://listserv.cc.kuleuven.ac.be/archives/chr.html').
user:url_path(bugzilla,  'http://gollem.science.uva.nl/bugzilla/').
user:url_path(gitweb,	 'http://www.swi-prolog.org/git/').
user:url_path(swieditor, 'http://lernen.bildung.hessen.de/informatik/swiprolog/indexe.htm').
user:url_path(pub,	 hcs('projects/SWI-Prolog/articles')).
user:url_path(git,	 'http://git-scm.com/').
user:url_path(msysgit,	 'http://code.google.com/p/msysgit/').
user:url_path(macports,	 'http://www.macports.org/').
user:url_path(xquartz,	 'http://xquartz.macosforge.org/').
user:url_path(json,	 'http://json.org/').
user:url_path(cliopatria, 'http://e-culture.multimedian.nl/software/ClioPatria.shtml').
user:url_path(multimedian, 'http://e-culture.multimedian.nl/').
user:url_path(thea,	 'http://www.semanticweb.gr/TheaOWLLib/').
user:url_path(dig,	 'http://dl.kr.org/dig/interface.html').
user:url_path(sparql,	 'http://www.w3.org/TR/rdf-sparql-query/').
user:url_path(serql,	 'http://www.openrdf.org').


		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

:- html_resource(plweb,
		 [ virtual(true),
		   requires([ pldoc_css,
			      css('plweb.css')
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
user:file_search_path(plgit,	     plweb(git)).
user:file_search_path(icons, 	     document_root(icons)).
user:file_search_path(cgi_bin,	     plweb('git-web')).
user:file_search_path(download,	     plweb(download)).
user:file_search_path(gitweb,	     plweb('git-web')).
