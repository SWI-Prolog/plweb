/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2024, VU University Amsterdam
			      SWI-Prolog Solutions b.v.

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
	  [ server/2,			% ?Role, ?Host
	    server/3			% ?Role, ?Host, -HostName
	  ]).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/html_head)).
:- use_module(library(www_browser)).
:- use_module(library(settings)).
:- use_module(library(pengines)).


:- setting(http:served_file_extensions,
	   list(atom),
	   [ html, gif, png, jpeg, jpg, css, js, tgz, exe, c, zip ],
	   'List of extensions that are served as plain files').
:- setting(http:index_files,
	   list(atom),
	   [ 'index.html', 'index.md' ],
	   'List of files that provide a directory index').
:- setting(http:port,
	   integer,
	   3040,
	   'Default port').
:- setting(http:workers,
	   integer,
	   10,
	   'Number of worker threads').

:- set_setting_default(pengines:allow_from, []).
:- set_setting_default(http:logfile, log('httpd.log')).
:- set_setting_default(http:cors, [*]).


		 /*******************************
		 *	     LOCATIONS		*
		 *******************************/

http:location(pldoc,	root(pldoc),	   [priority(10)]).
http:location(download,	root(download),	   []).
http:location(icons,	root(icons),	   []).
http:location(css,	root(css),	   []).
http:location(jq,	root('js/jquery'), []).


		 /*******************************
		 *	   EXTERNAL URLS	*
		 *******************************/

:- multifile
	user:url_path/2.

user:url_path(swi,	'/').
user:url_path(pkg,	swi('pldoc/package/')).
user:url_path(pack,	swi('pack/list/')).
user:url_path(swipub,	 swi('download/publications/')).
user:url_path(fsf,	'https://www.fsf.org').
user:url_path(gnu,	'https://www.gnu.org').
user:url_path(gpl,	gnu('licences/gpl.html')).
user:url_path(lgpl,	gnu('licences/lgpl.html')).
user:url_path(wordnet,	'https://wordnet.princeton.edu/').
user:url_path(gmp,	'https://gmplib.org/').
user:url_path(gitweb,	 'https://github.com/SWI-Prolog').
user:url_path(swieditor, 'https://arbeitsplattform.bildung.hessen.de/fach/informatik/swiprolog/indexe.html').
user:url_path(git,	 'https://git-scm.com/').
user:url_path(macports,	 'https://www.macports.org/').
user:url_path(xquartz,	 'https://www.xquartz.org/').
user:url_path(json,	 'https://json.org/').
user:url_path(thea,	 'http://vangelisv.github.io/thea/').
user:url_path(dig,	 'https://dl.kr.org/dig/').
user:url_path(sparql,	 'https://www.w3.org/TR/sparql11-query/').


		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

:- html_resource(swipl_css,
		 [ virtual(true),
		   requires([ css('swipl.css') ])
		 ]).
:- html_resource(plweb,
		 [ virtual(true),
		   requires([ pldoc_css,
			      css('plweb.css')
			    ])
		 ]).
:- if(\+html_current_resource(jquery)).
:- html_resource(jquery,
		 [ virtual(true),
		   requires([ jq('jquery.js')
			    ])
		 ]).
:- endif.
:- html_resource(js('jquery/ui/jquery-ui.min.js'),
		 [ requires([ jquery
			    ])
		 ]).
:- html_resource(jquery_ui,
		 [ virtual(true),
		   requires([ js('jquery/ui/jquery-ui.min.js'),
			      js('jquery/ui/jquery-ui.min.css')
			    ])
		 ]).
:- html_resource(jq('menu.js'),
		 [ requires([ jquery
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

user:file_search_path(data,          plweb(data)).
user:file_search_path(git_data,      data(git)).
user:file_search_path(git_data,      plweb(.)).
user:file_search_path(document_root, git_data(www)).
user:file_search_path(examples,      git_data(examples)).
user:file_search_path(blog,	     git_data(blog)).
user:file_search_path(private,       data(private)).
user:file_search_path(log,           data(log)).
user:file_search_path(download,	     data(download)).
user:file_search_path(icons,	     document_root(icons)).
user:file_search_path(css,	     document_root(css)).
user:file_search_path(js,	     document_root(js)).


		 /*******************************
		 *	   MASTER/SLAVE		*
		 *******************************/

%%	server(?Type, ?Host) is nondet.
%%	server(?Type, ?Host, ?HostName) is nondet.
%
%	Describe known servers  and  their   role.  Currently,  the only
%	important role is `master`. Logged in sessions are redirected to
%	the master to simplify keeping one view   of the data. In future
%	versions we might go for a more distributed database.

server(Type, Host) :-
	server(Type, Host, _HostName).

server(cdn,    'www.swi-prolog.org', -).
server(slave,  'us.swi-prolog.org', 'swi-prolog.osuosl.org').
server(master, 'eu.swi-prolog.org', -).
%server(master, 'localhost', -).
