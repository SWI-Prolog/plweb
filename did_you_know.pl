/*  Part of SWI-Prolog

    Author:        Anne Ogborn
    WWW:           http://www.swi-prolog.org

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

:-module(did_you_know, [did_you_know//0]).

/**  <module>  Interesting snippets about SWI-Prolog

This module generates the 'Did You Know   blah blah' bit that appears on
every pldoc and SWI-Prolog website page.
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(random)).
:- use_module(news).

:- use_module(library(julian)).

did_you_know -->
	{ maybe(0.5) },
	random_news, !.
did_you_know -->
	{ near_april_fools_day,
	  maybe(0.1)
	},
	random_silly_hint.
did_you_know -->
	random_hint.

/**  near_april_fools_day is semidet
 *
 * succeeds only within 12 hours either
 * side of April 1
 *
 * April fools day is a traditional holiday celebrated
 * by playing hoaxes and practical jokes. A common form
 * of this is to substitute nonsensical information where
 * useful info is normally displayed
 */
near_april_fools_day :-
	form_time([now, _-4-1]).
near_april_fools_day :-
	form_time([now, _-3-30, H:_:_]),
	H >= 12.
near_april_fools_day :-
	form_time([now, _-4-2, H:_:_]),
	H < 12.

random_silly_hint -->
	{ predicate_property(afdyk(_,_), number_of_clauses(N)),
	  (   debugging(dyk(Id)),
	      integer(Id)
	  ->  true
	  ;   random_between(1, N, Id)
	  ),
	  afdyk(Id, Saying)
	},
	say_af_saying(Saying).

say_af_saying(Text-Link) -->
	{ link(Link, HREF) },
	html([ span(class(lbl), 'Did you know?'),
	       ' ',
	       span(id(dyknow), a(href(HREF), Text))
	     ]).
say_af_saying(news(Text-Link)) -->
	{ link(Link, HREF) },
	html([ span(class(lbl), 'News:'),
	       ' ',
	       span(id(dyknow), a(href(HREF), Text))
	     ]).
say_af_saying(news(Text)) -->
	html([ span(class(lbl), 'News:'),
	       ' ',
	       span(id(dyknow), Text)
	     ]).
say_af_saying(Text) -->
	html([ span(class(lbl), 'Did you know?'),
	       ' ',
	       span(id(dyknow), Text)
	     ]).

random_hint -->
	{ predicate_property(dyk(_,_), number_of_clauses(N)),
	  (   debugging(dyk(Id)),
	      integer(Id)
	  ->  true
	  ;   random_between(1, N, Id)
	  ),
	  dyk(Id, Saying),
	  (   Saying = (Text-Link)
	  ->  link(Link, HREF),
	      Info = a(href(HREF), Text)
	  ;   Info = Saying
	  )
	},
	html([ span(class(lbl), 'Did you know?'),
	       ' ',
	       span(id(dyknow), Info)
	     ]).

link(section(Id), HREF) :- !,
	http_link_to_id(pldoc_man, [section=Id], HREF).
link(package(Name), HREF) :- !,
	file_name_extension(Name, html, File),
	http_link_to_id(pldoc_package, path_postfix(File), HREF).
link(pack(Name), HREF) :- !,
	http_link_to_id(pack_list, [p(Name)], HREF).
link(HREF, HREF).


%%	dyk(Id, Saying) is nondet
%
%	True if Saying is a DYK exression.  They come in two forms:
%
%	  * HTML - HREF
%	  * HTML
%
%	be careful with line lengths for sayings, 40 chars max

term_expansion(dyk(Saying), dyk(Id, Saying)) :-
	(   predicate_property(dyk(_,_), number_of_clauses(N))
	->  Id is N+1
	;   Id = 1
	).

dyk('SWI-Prolog is 28 years old').
dyk(['the ', b('Profiler'), ' can speed up your code']-section('profiling-predicates')).
dyk('You can hot-swap code').
dyk('SWI-Prolog uses logical update view'-section('update')).
dyk('M-/ does autocomplete in pceEmacs').
dyk('C-c C-c CamelCasesWords in pceEmacs').
dyk('C-c C-- underscores_words_in_pce_emacs').
dyk('C-+ and C-- changes font size in pceEmacs').
dyk('About the nifty drawing program pceDraw').
dyk('Special quasiquote syntax for html and javascript'-section('quasiquotations')).
dyk('Advice for choosing 32/64 bits'-section('64bits')).
dyk('You can configure your environment'-section('initfile')).
dyk(['SWI-Prolog supports the ', b('Snowball'), ' stemmer']-section('snowball')).
dyk('SWI-Prolog has an RDF Semantic Web Server'-'http://cliopatria.swi-prolog.org/home').
dyk('You can interface C++ to SWI-Prolog'-package('pl2cpp')).
dyk('SWI-Prolog can work with tar archives'-package('archive')).
dyk('This website is written entirely in SWI-Prolog'-package('http')).
dyk(['about the full featured ', b('web framework')]-package('http')).
dyk(['SWI-Prolog can act as an ', b('http client')]-section('http-clients')).
dyk('SWI-Prolog supports PDT, the Prolog Development Tools'-package('pdt')).
dyk('You can get Javadoc style documentation automatically'-package('pldoc')).
dyk(['SWI-Prolog has a ', b('unit test framework')]-package('plunit')).
dyk(['SWI-Prolog has a ', b('Natural Language Processing (NLP)'), ' library']-package('nlp')).
dyk(['SWI-Prolog supports ', b('Google Protocol Buffers')]-package('protobufs')).
dyk('SWI-Prolog talks to R'-package('R')).
dyk(['SWI-Prolog has ', b('powerful Semantic Web tools')]-package('semweb')).
dyk(['SWI-Prolog can ', b('parse SGML/XML')]-package('sgml')).
dyk(['SWI-Prolog has extensive ', b('GIS Support')]-package('space')).
dyk(['SWI-Prolog has support for ', b('large, static tables')]-package('table')).
dyk(['SWI-Prolog supports ', b('TIPC')]-package('tipc')).
dyk(['You can read/write ', b('.zip'), ' files']-package('zlib')).
dyk(['SWI-Prolog can talk to Java,C,C++,Python,and C#']-'/contrib/').
dyk(['You can control ', b('MIDI'), ' on Mac with SWI-Prolog']-'/contrib/SamerAbdallah/index.html').
dyk(['SWI-Prolog has ', b('an OpenGL Interface')]-'/contrib/OpenGL.html').
dyk(['SWI-Prolog is highly ', b('cross platform')]).
dyk('SWI-Prolog has multiple high quality random number generators'-'/contrib/SamerAbdallah/index.html').
dyk('The SWI-Prolog manual is available in printed form'-
    'http://books.google.nl/books/about/SWI_Prolog_Reference_Manual_6_2_2.html?id=q6R3Q3B-VC4C&redir_esc=y').
dyk(['ETALIS ', b('Event Processing'), ' runs on SWI-Prolog']-'http://code.google.com/p/etalis/').
dyk('This website\'s code is available'-'git://www.swi-prolog.org/home/pl/git/plweb.git').
dyk(['SWI-Prolog can talk to ', b('Matlab')]-'/contrib/SamerAbdallah/index.html').
dyk(['SWI-Prolog has an active ', b('mailing list')]-'/Mailinglist.html').
dyk(['Jan loves it when you ', b('Report Bugs')]-'/bugzilla/').
dyk(['You can get ', span(class=colored, 'COLORED'), ' text on the command line']-'/FAQ/ColorConsole.html').
dyk(['SWI-Prolog has a ', b('Nifty IDE')]-'/IDE.html').
dyk(['SWI-Prolog has a ', b('Graphic Debugger')]-'/gtrace.html').
dyk(['Try C-c C-n in pceEmacs']-'/navigator.html').
dyk('Try gxref. from the top level with a large project open'-'/gxref.html').
dyk('XPCE supports a sophisticated styled text engine').
dyk('Your proprietary application can use SWI-Prolog'-'/license.html').
dyk(['SWI-Prolog has an interface to FANN, a foss ', b('Neural Net'), ' library']-'http://leenissen.dk/fann/wp/').
dyk('SWI-Prolog has lots of useful Packages'-'/pack/list').
dyk(['SWI-Prolog can ', b('track open source licenses')]-section(softlicense)).
dyk(['SWI-Prolog has a pack to access ', b('Pubmed Data')]-pack(pubmed)).
dyk(['SWI-Prolog has ', b('Multi-Thread support')]-section(threads)).
dyk(['SWI-Prolog provides ', b('general DCG primitives')]-'/pldoc/doc/swi/library/dcg/basics.pl').
dyk('SWI-Prolog can handle Unix signals'-section(signal)).
dyk('SWI-Prolog can lazily parse a file'-section(pureinput)).
dyk('You can add menus to the swipl-win.exe console in windows'-section(plwin)).
dyk(['SWI-Prolog has a ', b('Profiler')]-section(profile)).
dyk('SWI-Prolog supports DDE on Windows'-section('DDE')).
dyk(['You can create ', b('stand alone exe files'), ' from SWI-Prolog code']-section(runtime)).
dyk('SWI-Prolog supports arbitrarily large integers').
dyk(['There\'s an API to interact with', b('Amazon')]-pack(amazon_api)).
dyk('Nifty call graphs'-pack(callgraph)).
dyk('condition is an alternative to exceptions'-pack(condition)).
dyk('You can train markov chains with BIMS'-pack(bims)).
dyk('blog_core is a nifty CMS for SWI-Prolog'-pack(blog_core)).
dyk('anything in single quotes is an atom').
dyk('Logtalk is now a pack'-pack(logtalk)).
dyk('probabilistic logic'-pack(cplint)).
dyk(['DCG help at lib ',
     a(href=location_by_id(section('dcg/basics')), 'dcg/basics'),
     ' + packs ',
     a(href=location_by_id(pack(dcg_util)), dcg_util), ' & ',
     a(href=location_by_id(pack(dcg_utils)), dcg_utils)]).
dyk('automatic UML->Prolog translation'-pack(dia)).
dyk('docstore is a document oriented DB in SWI-Prolog'-pack(docstore)).
dyk('you can deploy to dotcloud'-pack(dotcloud)).
dyk('CQL makes dealing with SQL easier'-section('/packages/cql.html')).
dyk('you can turn ugraphs into graphml'-pack(graphml)).
dyk('you can turn terms into graphviz (.dot) files'-pack(gvterm)).
dyk('about nifty JoCaml style multithreading'-pack(jolog)).
dyk('the first Prolog interpreter was in Fortran').
dyk('julian pack offers match based dates'-pack(julian)).
dyk('eliminate helper predicates with pack lambda'-pack(lambda)).
dyk('you can parse markdown').
dyk('don\'t use format to print errors'-section(debug)).
dyk('there\'s a simplex library'-section(simplex)).
dyk('use mavis for type checking'-pack(mavis)).
dyk('you can read ODF spreadsheets'-pack(odf_sheet)).


afdyk('SWI-Prolog complies with ISO JTC1/SC22/WG4'-'http://cobolstandard.info/wg4/wg4.html').
afdyk('C-c C-q automatically corrects syntax errors'-'/AprilFools.html').
afdyk('SWI-Prolog defaults to EBCDIC'-'/AprilFools.html').
afdyk(news('SWI-Prolog is now available on 9-track tape'-'/AprilFools.html')).
afdyk('pack_install(agi) installs Skynet'-'/AprilFools.html').
afdyk(['SWI-Prolog powers ', a(href='http://java.com' , 'this popular site')]).
afdyk('http_get is (?, ?, +)').
afdyk(news('A SWI-Prolog program beat the world champion in box hockey'-'/AprilFools.html')).
afdyk('8cD'-'https://github.com/Anniepoo/prolog-examples/blob/master/emoticons.pl').
afdyk(news('Bill Joy admits he\'s wrong, urges Prolog')).
afdyk('Prolog actually IS good for torturing undergrads').
afdyk(news('Colmerauer admits Prolog isn\'t logical at all')).
afdyk('about pack(antigravity)').
afdyk('SWI-Prolog 7.1.29 requires OSGi and qPID').
afdyk('this website runs on wind power'-'/dogfood.html').
afdyk('Nou breekt mijn klomp. Prolog is beter.').
afdyk('about pack(klomp)'-'/AprilFools.html').
afdyk('about pack(cheese)'-'/AprilFools.html').
afdyk('about pack(chocolate)'-'/AprilFools.html').


