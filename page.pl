/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2013, VU University Amsterdam

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

:- module(plweb_page, [sidebar//0]).
:- use_module(footer).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(pldoc/doc_index)).
:- use_module(library(pldoc/doc_search)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/doc_html), [object_name//2]).
:- use_module(wiki).
:- use_module(post).
:- use_module(openid).
:- use_module(did_you_know).

:- html_meta
	outer_container(+, html, ?, ?),
	outer_container(+, html, +, ?, ?).

:- http_handler(root(search), plweb_search, []).

%%	user:body(+Style, +Body)//
%
%	Provide the page skin.

:- multifile
	user:body//2.

user:body(homepage, Body) --> !,
	outer_container(_,
			[ \tag_line_area,
			  \menubar(fixed_width),
			  \blurb,
			  \cta_area,
			  \enhanced_search_area,
			  Body
			]).
user:body(user(Action), Body) --> !,
	outer_container(_,
			[ \title_area(user(Action)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, user])], Body)
			], false).
user:body(news(Which), Body) --> !,
	outer_container(_,
			[ \title_area(news(Which)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents,  news])], Body)
			]).
user:body(wiki(Special), Body) --> !,
	outer_container(_,
			[ \title_area(wiki(Special)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, wiki])], Body)
			]).
user:body(wiki(Path, Title), Body) --> !,
	outer_container(wiki(Path),
			[ \title_area(title(Title)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, wiki])], Body)
			]).
user:body(pack(Action), Body) --> !,
	outer_container(_,
			[ \title_area(pack(Action)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, wiki])], Body)
			]).
user:body(tags(Action), Body) --> !,
	outer_container(_,
			[ \title_area(tags(Action)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, tags])], Body)
			]).
user:body(pldoc(search(For)), Body) --> !,
	outer_container(_,
			[ \title_area(pldoc(search(For))),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, search])],
			      div(class(search), Body))
			]).
user:body(pldoc(Arg), Body) --> !,
	{ ignore(Arg = object(Obj)) },
	outer_container(Obj,
			[ \title_area(pldoc(Arg)),
			  \menubar(fixed_width),
			  div(class(breadcrumb), []),
			  div([id(contents), class([contents, pldoc])], Body)
			]).
user:body(plain, Body) --> !,
	html(body(class(plain), Body)).
user:body(default, Body) --> !,
	html(body(class(plain), Body)).
user:body(Style, _Body) -->
	html(div('Unknown page style ~q'-[Style])).

outer_container(Obj, Content) -->
	outer_container(Obj, Content, true).

outer_container(Obj, Content, ShowUser) -->
	html(body(div(class('outer-container'),
		  [ \html_requires(plweb),
		    \html_requires(swipl_css),
		    \shortcut_icons,
		    \upper_header,
		    Content,
		    div(class([footer, newstyle]), \footer(Obj, ShowUser)),
		    div(id('tail-end'), &(nbsp))
		  ]))),
	html_receive(script).


%%	prolog:doc_page_header(+File, +Options)//
%
%	Called to render the PlDoc page header. We kill the header.

:- multifile
	prolog:doc_page_header//2.

prolog:doc_page_header(_File, _Options) --> [].

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

upper_header -->
	{ http_link_to_id(plweb_search, [], Action) },
	html(div(id('upper-header'),
		 div(id('upper-header-contents'),
		     [ span(id('dyknow-container'), \did_you_know),
		       span(id('search-container'),
			    [ span(class(lbl), 'Search Documentation:'),
			      form([action(Action),id('search-form')],
				   [ input([ name(for),
					     id(for)
					   ], []),
				     input([ id('submit-for'),
					     type(submit),
					     value('Search')
					   ], []),
				     \searchbox_script(for)
				   ])
			    ])
		     ])
		)
	    ).

%%	plweb_search(+Request)
%
%	HTTP Handler to search the Prolog website.

plweb_search(Request) :-
	http_parameters(Request,
			[ for(For,
			      [ default(''),
				description('String to search for')
			      ])
			]),
	format(string(Title), 'Prolog search -- ~w', [For]),
	reply_html_page(pldoc(search(For)),
			title(Title),
			\search_reply(For,
				      [ resultFormat(summary),
					search_in(noapp),
					search_match(summary),
					header(false),
					edit(false)
				      ])).


%%	searchbox_script(+Tag)//
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
	html(div(id('tag-line-area'),
		 [ \swi_logo,
		   span(class(tagline),
			[ 'Robust, mature, free. ',
			  b('Prolog for the real world.')
			])
		 ])).

%
%	@arg	For provides information about the page displayed.
%		It is one of:
%
%		  - pldoc(object(Obj))
%		  PlDoc displays an object page

title_area(Arg) -->
	html(div(id('header-line-area'),
		 [ \swi_logo,
		   span(class('primary-header'),
			\page_title(Arg))
		 ])).

page_title(pldoc(search(''))) --> !,
	html('How to use the search box').
page_title(pldoc(search(For))) --> !,
	html(['Search results for ', span(class(for), ['"', For, '"'])]).
page_title(pldoc(object(Obj))) -->
	object_name(Obj,
		    [ style(title)
		    ]), !.
page_title(title(Title)) --> !,
	html(Title).
page_title(user(login)) --> !,
	html('Login to www.swi-prolog.org').
page_title(user(logout)) --> !,
	html('Logged out from www.swi-prolog.org').
page_title(user(create_profile)) --> !,
	html('Create user profile').
page_title(user(view_profile(UUID))) --> !,
	{ site_user_property(UUID, name(Name)) },
	html('Profile for user ~w'-[Name]).
page_title(news(fresh)) --> !,
	html('News').
page_title(news(all)) --> !,
	html('News archive').
page_title(news(Id)) -->
	{ post(Id, title, Title) },
	html(Title).
page_title(pack(list)) -->
	html('Packs (add-ons) for SWI-Prolog').
page_title(wiki(sandbox)) -->
	html('PlDoc wiki sandbox').
page_title(tags(list)) -->
	html('Tags').
page_title(Term) -->
	html('Title for ~q'-[Term]).


%%	swi_logo//
%
%	Embed the SWI-Prolog logo.

swi_logo -->
	{ http_absolute_location(icons('swipl.png'), Logo, []) },
	html(img([ class(owl),
		   src(Logo),
		   alt('SWI-Prolog owl logo')
		 ], [])).


%%	menubar(+Style:atom)// is semidet
%
%	Emits a menubar. Style must be one of full_width or fixed_width,
%	where full_width extends the yellow band   full  across the page
%	and fixed_width limits the yellow band  to 960px PageLocation is
%	the page location for editing

menubar(fixed_width) -->
	{  http_current_request(Request),
	   memberchk(request_uri(ReqURL), Request),
	   http_link_to_id(wiki_edit,
			   [location(ReqURL)], EditHREF),
	   http_link_to_id(plweb_login_page,
			   [openid.return_to(ReqURL)], LoginURL)
        },
	html([\html_requires(jquery),
	      \html({|html(LoginURL, EditHREF)||
    <div id='menubar'>
        <div class='menubar fixed-width'>
            <ul class='menubar-container'>
                <li><a href="/">HOME</a></li>
                <li>DOWNLOAD
                    <ul class='dropdown one'>
                        <li><a href="/Download.html">SWI-PROLOG</a></li>
                        <li><a href="/build/">SOURCES/BUILDING</a></li>
                        <li><a href="/pack/list">ADD-ONS</a></li>
                        <li><a href="/git/">BROWSE GIT</a></li>
                    </ul>
                </li>
                <li>DOCUMENTATION
                    <ul>
                        <li><a href="/pldoc/refman/">MANUAL</a></li>
                        <li><a href="/pldoc/package/">PACKAGES</a></li>
                        <li><a href="/FAQ/">FAQ</a></li>
                        <li><a href="/pldoc/man?section=cmdline">COMMAND LINE</a></li>
                        <li><a href="/pldoc/package/pldoc.html">PLDOC</a></li>
                        <li>BLUFFERS<span class='arrow'>&#x25B6;</span>
                            <ul>
                                <li><a href="/pldoc/man?section=syntax">PROLOG SYNTAX</a></li>
                                <li><a href="/pldoc/man?section=emacsbluff">pceEMACS</a></li>
                                <li><a href="/pldoc/man?section=htmlwrite">HTML GENERATION</a></li>
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
                                <li><a href="http://michael.richter.name/tutorials/swiplmod">MODULES</a></li>
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
                                <li><a href="/build/guidelines.txt">LINUX PACKAGES</a></li>
                            </ul>
                        </li>
                    </ul>
                </li><!-- tutorials -->
                <li>COMMUNITY
                    <ul>
                        <li><a href="/Support.txt">SUPPORT</a></li>
                        <li><a href="http://webchat.freenode.net/?channels=##prolog">IRC</a></li>
                        <li><a href="/news">NEWS</a></li>
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
                        <li><a href="/students/index.html">STUDENTS</a></li>
                        <li><a href="/research/index.html">RESEARCHERS</a></li>
                        <li><a href="/commercial/index.html">COMMERCIAL USERS</a></li>
                        <li><a href="/dogfood.html">DOG FOOD</a></li>
                    </ul>
                </li><!-- users -->
                <li>WIKI
                    <ul>
                        <li><a href="LoginURL">LOGIN</a></li>
                        <li><a href=EditHREF>EDIT THIS PAGE</a></li>
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

%%	blurb//
%
%	Emit the blurb
blurb -->
	html({|html||
    <div id="blurb">
      <div>
	 SWI-Prolog offers a comprehensive free Prolog environment.
	 Since its start in 1987, SWI-Prolog development has been driven
	 by the needs of real world applications. SWI-Prolog is widely
	 used in research and education as well as commercial applications.
	 Join over a million users who have downloaded SWI-Prolog.
      </div>
    </div>
	     |}).


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
	{ http_link_to_id(plweb_search, [], Action) },
	html({|html(Action)||
	      <div id='enhanced-search-container'>
	        <div>
	          <span class='lbl'>SEARCH DOCUMENTATION:</span>
	          <form  id="search-form-enhanced" action="Action">
	            <input type='text' id="forenhanced">
	            <input type="image" src="/icons/go.png" alt='Search'>
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

