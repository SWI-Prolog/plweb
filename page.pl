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

:- module(plweb_page,
	  [ sidebar//0,
	    server_address//0
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(pldoc/doc_index)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(wiki).
:- use_module(openid).
:- use_module(did_you_know).

%%	user:body(+Style, +Body)//
%
%	Redefine body behaviour

:- multifile
	user:body//2.

user:body(homepage, Body) -->
	!,
	{
	    % TBD Yikes, Jan, how do we find this here?
	    PageLocation = /
        },
	html(body(div(class('outer-container'),
	          [ \html_requires(plweb),
		    \html_requires(swipl_css),
		    \upper_header,
		    \tag_line_area,
		    \menubar(fixed_width, PageLocation),
		    \blurb,
		    \cta_area,
		    \enhanced_search_area,
		    Body,
		    div(id('tail-end'), &(nbsp))
		  ]))),
	html_receive(script).

user:body(wiki, Body) --> !,
	user:body(wiki(default), Body).
% serves index among other things
user:body(wiki(Arg), Body) --> !,
	{
	    % TBD Yikes, Jan, how do we find this here?
	    PageLocation = /
        },
	html(body(div(class('outer-container'),
		  [ \html_requires(plweb),
		    \html_requires(swipl_css),
		    \shortcut_icons,
		    \upper_header,
		    \title_area,
		    \menubar(fixed_width, PageLocation),
		    \page_extra(Arg),
		    p('Someday breadcrumb'),
		    % \doc_links([], [search_options(false)]),
		    div(id(contents), div(Body)),
		    div(class('footer newstyle'), [\current_user(Arg), \server_address]),
		    div(id('tail-end'), &(nbsp))
		  ]))),
	html_receive(script).
user:body(plain, Body) --> !,
	html(body(class(wiki), [h1(plain), Body])).   % AO h1 is DEBUG
user:body(_, Body) -->
	html(body(class(pldoc),
		  [ \html_requires(plweb),
		    \shortcut_icons,
		    div(class(sidebar), \sidebar),
		    div(class(righthand),
			[ \current_user,
			  h1(pldoc),  % AO DEBUG
			  div(class(content), Body),
			  div(class(footer), \server_address)
			])
		  ])),
	html_receive(script).

shortcut_icons -->
	{ http_absolute_location(icons('favicon.ico'), FavIcon, []),
	  http_absolute_location(root('apple-touch-icon.png'), TouchIcon, [])
	},
	html_post(head,
		  [ link([ rel('shortcut icon'), href(FavIcon) ]),
		    link([ rel('apple-touch-icon'), href(TouchIcon) ])
		  ]).

%%	upper_header//
%
%	Emit the small blue header with Did You Know? and search box
%
upper_header -->
  html(
    div(id('upper-header'), [
      div(id('upper-header-contents'),
        span(id('dyknow-container'), \did_you_know)
      ),
      span(id('search-container'), [
        span(class(lbl), 'Search Documentation:'),
        form([action('/pldoc/search'),id('search-form')], [
          input([name(for), id(for)], []),
          input([id('submit-for'), type(submit), value('Search')], []),
          input([type(hidden), name(in), value(all)], []),
          input([type(hidden), name(match), value(summary)], []),
          \searchbox_script(for)
        ])
      ])
    ])
  ).

%%	searchbox_script//
%
%	Emits the script tag for the searchbox
searchbox_script(Tag) -->
	html([
	    \html_requires(jquery_ui),
	    script(type('text/javascript'), {|javascript(Tag)||
    $(function() {
        $("#"+Tag).autocomplete({
        minLength: 1,
        delay: 0.3,
        source: "/autocomplete/ac_predicate",
        focus: function(event,ui) {
          $("#"+Tag).val(ui.item.label);
          return false;
        },
        select: function(event,ui) {
          $("#"+Tag).val(ui.item.label);
          window.location.href = ui.item.href;
          return false;
        }
        })
        .data("ui-autocomplete")._renderItem = function(ul,item) {
        var label = String(item.label).replace(
            new RegExp(this.term),
            "<span class=\"acmatch\">$&</span>");
        var tag = item.tag ? " <i>["+item.tag+"]</i>" : "";
        return $("<li>")
          .append("<a class=\""+item.class+"\">"+label+tag+"</a>")
          .appendTo(ul)
        };
        });
|})]).

%%	tag_line_area//
%
%	Emit the Owl logo and tagline area (Robust, mature, free. Prolog
%	for the real world)
tag_line_area -->
	html(div(id('tag-line-area'), [
	    img(src('icons/swipl.png'), []),
	    span(class(tagline), ['Robust, mature, free. ', b('Prolog for the real world.')])
	])).

%%	title_area//
%
%	Emit the Owl logo and page title
%
%	@tbd receive title via mailman
%
title_area -->
	{
                    Title = 'Yet To Do'  % TBD recieve this via mailman
        },
	html(div(id('header-line-area'), [
		     img(src('icons/swipl.png'),[]),
		     span(class('primary-header'), Title)
		 ])).


%%	menubar(+Style:atom, +PageLocation:atom)// is semidet
%
%	Emits a menubar. Style must be one of full_width or fixed_width,
%	where full_width extends the yellow band full across the page
%	and fixed_width limits the yellow band to 960px
%	PageLocation is the page location for editing
%
:- style_check(-atom).
menubar(fixed_width, PageLocation) -->
	{
           uri_query_components(Str, [location=PageLocation]),
           format(atom(PageHREF), '/wiki_edit?~w', [Str])
        },
	html([\html_requires(jquery),
	      \html({|html(PageHREF)||
    <div id='menubar'>
        <div class='menubar fixed-width'>
            <ul class='menubar-container'>
                <li><a href="/">HOME</a></li>
                <li>DOWNLOAD
                    <ul class='dropdown one'>
                        <li><a href="/Download.html">SWI-PROLOG</a></li>
                        <li><a href="/sources.html">SOURCES/BUILDING</a></li>
                        <li><a href="/addons.html">ADD-ONS</a></li>
                        <li><a href="/git/">BROWSE GIT</a></li>
                    </ul>
                </li>
                <li>DOCUMENTATION
                    <ul>
                        <li><a href="/pldoc/refman/">MANUAL</a></li>
                        <li><a href="/pldoc/index.html">PACKAGES</a></li>
                        <li><a href="/FAQ/">FAQ</a></li>
                        <li><a href="/pldoc/man?section=cmdline">COMMAND LINE</a></li>
                        <li><a href="/pldoc/package/pldoc.html">PLDOC</a></li>
                        <li>BLUFFERS<span class='arrow'>&#x25B6;</span>
                            <ul>
                                <li><a href="/prologsyntax.html">PROLOG SYNTAX</a></li>
                                <li><a href="/pldoc/man?section=emacsbluff">pceEMACS</a></li>
                                <li><a href="/htmlbluffer.html">HTML GENERATION</a></li>
                            </ul>
                        </li>
                        <li><a href="/license.html">LICENSE</a></li>
                        <li><a href="/Contributors.html">CONTRIBUTORS</a></li>
                        <li><a href="/Publications.html">PUBLICATIONS</a></li>
                    </ul>
                </li>
                <li>TUTORIALS
                    <ul>
                        <li>BEGINNER<span class='arrow'>&#x25B6;</span>
                            <ul>
                                <li><a href="/pldoc/man?section=quickstart">GETTING STARTED</a></li>
                                <li><a href="/swipltuts/mod/index.html">MODULES</a></li>
                                <li><a href="/pldoc/man?section=debugoverview">DEBUGGER</a></li>
                                <li><a href="/IDE.html">DEVELOPMENT TOOLS</a></li>
                            </ul>
                        </li>
                        <li>INTERMEDIATE<span class='arrow'>&#x25B6;</span>
                            <ul>
                                <li><a href="/howto/http/">WEB SERVICE</a></li>
                                <li><a href="/swipltuts/html/index.html">WEB APPLICATIONS</a></li>
                                <li><a href="/swipltuts/pldoc/index.html">PLDOC</a></li>
                                <li><a href="/dcg/index.html">DCGs</a></li>
                                <li><a href="/xpce.pdf">XPCE</a></li>
                                <li><a href="/Graphics.html">GUI OPTIONS</a></li>
                            </ul>
                        </li>
                        <li>ADVANCED<span class='arrow'>&#x25B6;</span>
                            <ul>
                                <li><a href="/howto/UseRdfMeta.html">RDF NAMESPACES</a></li>
                                <li><a href="/build/guidelines.txt">UNIX PACKAGES</a></li>
                            </ul>
                        </li>
                    </ul>
                </li><!-- tutorials -->
                <li>COMMUNITY
                    <ul>
                        <li><a href="/Support.txt">SUPPORT</a></li>
                        <li><a href="http://webchat.freenode.net/?channels=##prolog">IRC</a></li>
                        <li><a href="/Mailinglist.txt">MAIL LIST</a></li>
                        <li><a href="/bugzilla/">BUG TRACKER</a></li>
                        <li><a href="/Links.html">EXTERNAL LINKS</a></li>
                        <li><a href="/Contact.html">CONTACT</a></li>
                        <li><a href="/loot.html">SWI-PROLOG ITEMS</a></li>
                    </ul>
                </li><!-- community -->
                <li>USERS
                    <ul>
                        <li><a href="/web/index.html">SEMANTIC WEB</a></li>
                        <li><a href="/swipltuts/student/index.html">STUDENTS</a></li>
                        <li><a href="/Publications.html">RESEARCHERS</a></li>
                        <li><a href="/commercial.html">COMMERCIAL USERS</a></li>
                    </ul>
                </li><!-- users -->
                <li>WIKI
                    <ul>
                        <li><a href="/openid/login?openid.return_to=/user/logout">LOGIN</a></li>
                        <li><a href=PageHREF>EDIT THIS PAGE</a></li>
                        <li><a href="/wiki/sandbox">SANDBOX</a></li>
                        <li><a href="/wiki/">WIKI HELP</a></li>
                        <li><a href="/list-tags">ALL TAGS</a></li>
                    </ul>
                </li><!-- wiki -->
            </ul>
        </div>
    </div>|}),
	      \html_requires(jq('menu.js'))
	]).
:- style_check(+atom).


%%	blurb//
%
%	Emit the blurb
blurb -->
	html(div(id(blurb), div(
'SWI-Prolog offers a comprehensive free Prolog environment. Since its start in 1987, SWI-Prolog development has been driven by the needs of real world applications. SWI-Prolog is widely used in research and education as well as commercial applications. Join over a million users who have downloaded SWI-Prolog.'
			    ))).

%%	cta_area//
%
%	Emit the Call To Action - the 3 big buttons on homepage
cta_area -->
	html({|html(_)||
    <div id='cta-container'>
        <div>Download SWI-Prolog</div>
        <div>Get Started</div>
        <div>Is SWI-Prolog Right For My Project?</div>
    </div>
    <div>&nbsp;</div>|}).


%%	enhanced_search_area//
%
%	Emit the large size search area at bottom of home page
%
enhanced_search_area -->
	html({|html(_)||    <div id='enhanced-search-container'>
        <div>
            <span class='lbl'>SEARCH DOCUMENTATION:</span>
            <form  id="search-form-enhanced" action="/pldoc/search">
                <input type='text' id="forenhanced">
                <input type="image" src="/icons/go.png" alt='Search'>
                <input type="hidden" name="in" value="all">
                <input type="hidden" name="match" value="summary">
            </form>
        </div>
    </div>|}),
	searchbox_script(forenhanced).


%         ================ Old version resumes =========================


%%	sidebar//
%
%	Emit the sidebar with logo and menu

sidebar -->
	html([ div(class(logo),
		   a(href('http://www.swi-prolog.org'),
		     img([id(logo),
			  border(0),
			  alt('SWI-Prolog logo'),
			  src('/icons/swipl.png')
			 ]))),
	       div(class(menu), \menu)
	     ]).

%%	menu//
%
%	Generate the sidebar menu

menu -->
	{ menu(DOM) },
	html(DOM).

menu(DOM) :-
	nb_current(pldoc_file, OrgFile),
	menu_file(OrgFile, MenuFile), !,
	wiki_file_to_dom(MenuFile, DOM).
menu(DOM) :-
	absolute_file_name(document_root('menu.txt'),
			   MenuFile,
			   [ access(read)
			   ]),
	wiki_file_to_dom(MenuFile, DOM).
menu([]).

menu_file(Base, MenuFile) :-
	parent(Base, Dir), Dir \== Base,
	concat_atom([Dir, /, 'menu.txt'], MenuFile),
	exists_file(MenuFile).

parent(Base, Base).
parent(Base, Parent) :-
	file_directory_name(Base, Dir),
	Dir \== Base,
	parent(Dir, Parent).


%%	server_address//
%
%	Emit information about the server

server_address -->
	{ prolog_version(Version)
	},
	html(['Powered by ',
	      a(href('http://www.swi-prolog.org'), 'SWI-Prolog'), ' ',
	      Version
	     ]).

prolog_version(Version) :-
	current_prolog_flag(version_git, Version), !.
prolog_version(Version) :-
	current_prolog_flag(version_data, swi(Ma,Mi,Pa,_)),
	format(atom(Version), '~w.~w.~w', [Ma,Mi,Pa]).

page_extra(home) --> !,
	html({|html||
<style type="text/css">
#owl-hdr {
    text-align: center;
    margin: auto;
    width: auto;
}

#owl-hdr span
{ vertical-align: middle;
  font-size: 200%;
  font-weight: bold;
  font-style: italic;
}

#owls {
    background: url(/icons/3owls.jpeg) no-repeat;
    height: 100px;
    width: 256px;
    display: inline-block;
}

#owls:hover {
    background: url(/icons/logtalk.jpeg)  no-repeat;
}
</style>

<div id="owl-hdr">
  <span>Happy</span>
  <span id="owls"></span>
  <span>Holidays</span>
</div>
	     |}).
page_extra(_) -->
	[].
