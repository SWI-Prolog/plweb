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
  {
    NewsPercentage = 0.5,
    random(R)
  },
  (
    {R < NewsPercentage},
    random_news, !
  ;
    random_hint
  ).

random_hint -->
  {
    dyk_sayings(Sayings),
    random_member(Saying-Link, Sayings),
    (
       Link = none
    ->
       Info = Saying
    ;
       Info = a(href=Link, Saying)
    )
  },
  html([
    span(class(lbl), 'Did you know?'),
    ' ',
    span(id(dyknow), Info)
  ]).


%%	 dyk_sayings(-Sayings:list) is det
%
%	returns a list of Saying-Link pairs
%
% be careful with line lengths for sayings, 40 chars max
%
dyk_sayings([
    'SWI-Prolog is 25 years old'-none,
    ['the ', b('Profiler'), ' can speed up your code']-'/pldoc/man?section=profiling-predicates',
    'You can hot-swap code'-none,
    'SWI-Prolog uses logical update view'-'/pldoc/man?section=update',
    'M-/ does autocomplete in pceEmacs'-none,
    'C-c C-c CamelCasesWords in pceEmacs'-none,
    'C-c C-- underscores_words_in_pce_emacs'-none,
    'C-+ and C-- changes font size in pceEmacs'-none,
    'About the nifty drawing program pceDraw'-none,  % TBD make a page for this
    'Special quasiquote syntax for html and javascript'-'/pldoc/man?section=quasiquotations',
    'Advice for choosing 32/64 bits'-'/pldoc/man?section=64bits',
    'You can configure your environment'-'/pldoc/man?section=initfile',
    ['SWI-Prolog supports the ', b('Snowball'), ' stemmer']-'/pldoc/man?section=snowball',
    'SWI-Prolog has an RDF Semantic Web Server'-'http://cliopatria.swi-prolog.org/home',
    'You can interface C++ to SWI-Prolog'-'/pldoc/package/pl2cpp.html',
    'SWI-Prolog can work with tar archives'-'/pldoc/package/archive.html',
    'This website is written entirely in SWI-Prolog'-'/pldoc/package/http.html',
    ['about the full featured ', b('web framework')]-'/pldoc/package/http.html',
    ['SWI-Prolog can act as an ', b('http client')]-'/pldoc/doc_for?object=section(2,''2'',swi(''/doc/packages/http.html''))',
    'SWI-Prolog supports PDT, the Prolog Development Tools'-'/pldoc/package/pdt.html',
    'You can get Javadoc style documentation automatically'-'/pldoc/package/pldoc.html',
    ['SWI-Prolog has a ', b('unit test framework')]-'/pldoc/package/plunit.html',
    ['SWI-Prolog has a ', b('Natural Language Processing (NLP)'), ' library']-'/pldoc/package/nlp.html',
    ['SWI-Prolog supports ', b('Google Protocol Buffers')]-'/pldoc/package/protobufs.html',
    'SWI-Prolog talks to R'-'/pldoc/package/R.html',
    ['SWI-Prolog has ', b('powerful Semantic Web tools')]-'/pldoc/package/semweb.html',
    ['SWI-Prolog can ', b('parse SGML/XML')]-'/pldoc/package/sgml.html',
    ['SWI-Prolog has extensive ', b('GIS Support')]-'/pldoc/package/space.html',
    ['SWI-Prolog has support for ', b('large, static tables')]-'/pldoc/package/table.html',
    ['SWI-Prolog supports ', b('TIPC')]-'/pldoc/package/tipc.html',
    ['You can read/write ', b('.zip'), ' files']-'/pldoc/package/zlib.html',
    ['SWI-Prolog can talk to Java,C,C++,Python,and C#']-'/contrib/',
    ['You can control ', b('MIDI'), ' on Mac with SWI-Prolog']-'/contrib/SamerAbdallah/index.html',
    ['SWI-Prolog has ', b('an OpenGL Interface')]-'/contrib/OpenGL.html',
    ['SWI-Prolog is highly ', b('cross platform')]-none,
    'SWI-Prolog has multiple high quality random number generators'-'/contrib/SamerAbdallah/index.html',
    'The SWI-Prolog manual is available in printed form'-
    'http://books.google.nl/books/about/SWI_Prolog_Reference_Manual_6_2_2.html?id=q6R3Q3B-VC4C&redir_esc=y',
    ['ETALIS ', b('Event Processing'), ' runs on SWI-Prolog']-'http://code.google.com/p/etalis/',
    'This website\'s code is available'-'git://www.swi-prolog.org/home/pl/git/plweb.git',
    ['SWI-Prolog can talk to ', b('Matlab')]-'/contrib/SamerAbdallah/index.html',
    ['SWI-Prolog has an active ', b('mailing list')]-'/Mailinglist.html',
['Jan loves it when you ', b('Report Bugs')]-'/bugzilla/',
    ['You can get ', span(class=colored, 'COLORED'), ' text on the command line']-'/FAQ/ColorConsole.html',
    ['SWI-Prolog has a ', b('Nifty IDE')]-'/IDE.html',
    ['SWI-Prolog has a ', b('Graphic Debugger')]-'/gtrace.html',
    ['Try C-c C-n in pceEmacs']-'/navigator.html',
    'Try gxref. from the top level with a large project open'-'/gxref.html',
    'XPCE supports a sophisticated styled text engine'-none,
    'Your proprietary application can use SWI-Prolog'-'/license.html',
    ['SWI-Prolog has an interface to FANN, a foss ', b('Neural Net'), ' library']-'http://leenissen.dk/fann/wp/',
    'SWI-Prolog has lots of useful Packages'-'/pack/list',
    ['SWI-Prolog can ', b('track open source licenses')]-'/pldoc/doc_for?object=section(2,''E.3'',swi(''/doc/Manual/softlicense.html''))',
    ['SWI-Prolog has a pack to access ', b('Pubmed Data')]-'/pack/list?p=pubmed',
    ['SWI-Prolog has ', b('Multi-Thread support')]-'/man/threads.html',
    ['SWI-Prolog provides ', b('general DCG primitives')]-'/pldoc/doc/swi/library/dcg/basics.pl',
    'SWI-Prolog can handle Unix signals'-'/pldoc/doc_for?object=section(2,''4.11'',swi(''/doc/Manual/signal.html''))',
    'SWI-Prolog can lazily parse a file'-'/pldoc/doc_for?object=section(2,''A.19'',swi(''/doc/Manual/pio.html''))',
    'You can add menus to the swipl-win.exe console in windows'-'/pldoc/doc_for?object=section(3,''4.33.2'',swi(''/doc/Manual/system.html''))',
    ['SWI-Prolog has a ', b('Profiler')]-'/pldoc/doc_for?object=section(2,''4.39'',swi(''/doc/Manual/profile.html''))',
    'SWI-Prolog supports DDE on Windows'-'/pldoc/doc_for?object=section(2,''4.41'',swi(''/doc/Manual/DDE.html''))',
    ['You can create ', b('stand alone exe files'), ' from SWI-Prolog code']-'/pldoc/doc_for?object=section(1,''10'',swi(''/doc/Manual/runtime.html''))',
    'SWI-Prolog supports arbitrarily large integers'-none
	    ]).
