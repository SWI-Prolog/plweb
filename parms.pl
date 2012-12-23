/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

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
user:url_path(pkg,	pl(package)).
user:url_path(dmoz,	'http://dmoz.org').
user:url_path(dmoz_pl,	dmoz('Computers/Programming/Languages/Prolog/Implementations/')).
user:url_path(ffii,	'http://swpat.ffii.org/index.en.html').
user:url_path(fsf,	'http://www.fsf.org').
user:url_path(gnu,	'http://www.gnu.org').
user:url_path(gpl,	gnu('licences/gpl.html')).
user:url_path(lgpl,	gnu('licences/lgpl.html')).
user:url_path(wordnet,	'http://www.cogsci.princeton.edu/~wn/').
user:url_path(gmp,	'http://gmplib.org/').
user:url_path(mailman,	'http://www.list.org/').
user:url_path(bonn,	'https://lists.iai.uni-bonn.de/mailman').
user:url_path(maillist,	bonn('listinfo.cgi/swi-prolog')).
user:url_path(mailarchive, bonn('public/swi-prolog/')).
user:url_path(nabble,	'http://www.nabble.com').
user:url_path(pl_nabble, nabble('SWI-Prolog-f448.html')).
user:url_path(gmane,     'http://blog.gmane.org').
user:url_path(pl_gmane,  gmane('gmane.comp.ai.prolog.swi')).
user:url_path(chr_mail,  'http://listserv.cc.kuleuven.ac.be/archives/chr.html').
user:url_path(bugzilla,  'http://www.swi-prolog.org/bugzilla/').
user:url_path(gitweb,	 'http://www.swi-prolog.org/git/').
user:url_path(swieditor, 'http://lakk.bildung.hessen.de/netzwerk/faecher/informatik/swiprolog/indexe.html').
user:url_path(pub,	 hcs('projects/SWI-Prolog/articles')).
user:url_path(swipub,	 swi('download/publications')).
user:url_path(git,	 'http://git-scm.com/').
user:url_path(msysgit,	 'http://code.google.com/p/msysgit/').
user:url_path(tortoisegit, 'http://code.google.com/p/tortoisegit/').
user:url_path(macports,	 'http://www.macports.org/').
user:url_path(xquartz,	 'http://xquartz.macosforge.org/').
user:url_path(json,	 'http://json.org/').
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
user:file_search_path(icons,	     document_root(icons)).
user:file_search_path(css,	     document_root(css)).
user:file_search_path(yui,	     document_root('yui/2.7.0')).
user:file_search_path(cgi_bin,	     plweb('git-web')).
user:file_search_path(download,	     plweb(download)).
user:file_search_path(gitweb,	     plweb('git-web')).
