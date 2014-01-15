:-module(did_you_know, [did_you_know//0]).
/**  <module>  Interesting snippets about SWI-Prolog

  This module generates the 'Did You Know blah blah' bit that
  appears on every pldoc and SWI-Prolog website page.

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

    @author Anne Ogborn
    @tbd Let Jessica know about b and color

*/
:- use_module(library(http/html_write)).
:- use_module(library(random)).
:- use_module(news).

did_you_know -->
	{ maybe(0.5) }, !,
	random_news.
did_you_know -->
	random_hint.

random_hint -->
	{ predicate_property(dyk(_,_), number_of_clauses(N)),
	  random_between(1, N, Id),
	  dyk(Id, Saying),
	  (   Saying = (Text-Link)
	  ->  Info = a(href=Link, Text)
	  ;   Info = Saying
	  )
	},
	html([ span(class(lbl), 'Did you know?'),
	       ' ',
	       span(id(dyknow), Info)
	     ]).


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

dyk('SWI-Prolog is 25 years old').
dyk(['the ', b('Profiler'), ' can speed up your code']-'/pldoc/man?section=profiling-predicates').
dyk('You can hot-swap code').
dyk('SWI-Prolog uses logical update view'-'/pldoc/man?section=update').
dyk('M-/ does autocomplete in pceEmacs').
dyk('C-c C-c CamelCasesWords in pceEmacs').
dyk('C-c C-- underscores_words_in_pce_emacs').
dyk('C-+ and C-- changes font size in pceEmacs').
dyk('About the nifty drawing program pceDraw').
dyk('Special quasiquote syntax for html and javascript'-'/pldoc/man?section=quasiquotations').
dyk('Advice for choosing 32/64 bits'-'/pldoc/man?section=64bits').
dyk('You can configure your environment'-'/pldoc/man?section=initfile').
dyk(['SWI-Prolog supports the ', b('Snowball'), ' stemmer']-'/pldoc/man?section=snowball').
dyk('SWI-Prolog has an RDF Semantic Web Server'-'http://cliopatria.swi-prolog.org/home').
dyk('You can interface C++ to SWI-Prolog'-'/pldoc/package/pl2cpp.html').
dyk('SWI-Prolog can work with tar archives'-'/pldoc/package/archive.html').
dyk('This website is written entirely in SWI-Prolog'-'/pldoc/package/http.html').
dyk(['about the full featured ', b('web framework')]-'/pldoc/package/http.html').
dyk(['SWI-Prolog can act as an ', b('http client')]-'/pldoc/doc_for?object=section(2,''2'',swi(''/doc/packages/http.html''))').
dyk('SWI-Prolog supports PDT, the Prolog Development Tools'-'/pldoc/package/pdt.html').
dyk('You can get Javadoc style documentation automatically'-'/pldoc/package/pldoc.html').
dyk(['SWI-Prolog has a ', b('unit test framework')]-'/pldoc/package/plunit.html').
dyk(['SWI-Prolog has a ', b('Natural Language Processing (NLP)'), ' library']-'/pldoc/package/nlp.html').
dyk(['SWI-Prolog supports ', b('Google Protocol Buffers')]-'/pldoc/package/protobufs.html').
dyk('SWI-Prolog talks to R'-'/pldoc/package/R.html').
dyk(['SWI-Prolog has ', b('powerful Semantic Web tools')]-'/pldoc/package/semweb.html').
dyk(['SWI-Prolog can ', b('parse SGML/XML')]-'/pldoc/package/sgml.html').
dyk(['SWI-Prolog has extensive ', b('GIS Support')]-'/pldoc/package/space.html').
dyk(['SWI-Prolog has support for ', b('large, static tables')]-'/pldoc/package/table.html').
dyk(['SWI-Prolog supports ', b('TIPC')]-'/pldoc/package/tipc.html').
dyk(['You can read/write ', b('.zip'), ' files']-'/pldoc/package/zlib.html').
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
dyk(['SWI-Prolog can ', b('track open source licenses')]-'/pldoc/doc_for?object=section(2,''E.3'',swi(''/doc/Manual/softlicense.html''))').
dyk(['SWI-Prolog has a pack to access ', b('Pubmed Data')]-'/pack/list?p=pubmed').
dyk(['SWI-Prolog has ', b('Multi-Thread support')]-'/man/threads.html').
dyk(['SWI-Prolog provides ', b('general DCG primitives')]-'/pldoc/doc/swi/library/dcg/basics.pl').
dyk('SWI-Prolog can handle Unix signals'-'/pldoc/doc_for?object=section(2,''4.11'',swi(''/doc/Manual/signal.html''))').
dyk('SWI-Prolog can lazily parse a file'-'/pldoc/doc_for?object=section(2,''A.19'',swi(''/doc/Manual/pio.html''))').
dyk('You can add menus to the swipl-win.exe console in windows'-'/pldoc/doc_for?object=section(3,''4.33.2'',swi(''/doc/Manual/system.html''))').
dyk(['SWI-Prolog has a ', b('Profiler')]-'/pldoc/doc_for?object=section(2,''4.39'',swi(''/doc/Manual/profile.html''))').
dyk('SWI-Prolog supports DDE on Windows'-'/pldoc/doc_for?object=section(2,''4.41'',swi(''/doc/Manual/DDE.html''))').
dyk(['You can create ', b('stand alone exe files'), ' from SWI-Prolog code']-'/pldoc/doc_for?object=section(1,''10'',swi(''/doc/Manual/runtime.html''))').
dyk('SWI-Prolog supports arbitrarily large integers').
