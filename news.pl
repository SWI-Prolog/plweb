:- module(news, [random_news//0]).

/** <module> News on the SWI-Prolog Web site

    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

    @author Wouter Beek
    @tbd Calculate relevance based on freshness lifetime and importance.
    @tbd User-specific influencing of relevance. Based on login
         / based on cookies.
    @version 2013/12
*/

:- use_module(generics).
:- use_module(library(aggregate)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json_convert)).
:- use_module(post).

:- html_resource(css('news.css'), [requires([css('post.css')])]).

http:location(news, root(news), []).	% used for write_post_js//2.
:- http_handler(root(news), news_process, [prefix]).
:- http_handler(root(news/archive), news_archive, []).


news_process(Request):-
  memberchk(method(Method), Request),
  news_process(Method, Request).

% HTTP GET on a specific news item.
news_process(get, Request):-
  request_to_resource(Request, URL), !,
  http_get(URL, JSON, []),
  json_to_prolog(JSON, post:Post),
  post(Post, title, Title1),
  post(Post, id, Id),
  atomic_list_concat(['News',Title1], ' -- ', Title2),
  reply_html_page(
    news(Id),
    title(Title2),
    [\news_backlink,\post([], null, Id)]
  ).
% HTTP GET without a specific news item: enumerate all fresh news item.
news_process(get, _):- !,
  find_posts(news, fresh, Ids),
  Title = 'News',
  reply_html_page(news(fresh), \news_header(Title), \posts(news, null, Ids)).
% HTTP methods other than GET go via the REST API for generic posts.
news_process(Method, Request):-
  request_to_id(Request, Id),
  post:rest_process(Method, Request, Id).

news_header(Title) -->
  html([
    \html_requires(css('news.css')),
    title(Title)
  ]).

% The list of fresh and stale (i.e., all) news items.
news_archive(_Request):-
  find_posts(news, all, Ids),
  Title = 'News archive',
  reply_html_page(news(all), title(Title), \posts(news, null, Ids)).

news_backlink -->
  { http_link_to_id(news_process, [], Link) },
  html(a(href=Link, 'Back to list of news items')).

%! random_news// is semidet.
% Fails if there is no news.

random_news -->
  { '_random_news'(Id, Title),
    http_link_to_id(news_process, path_postfix(Id), Link)
  },
  html([
    span(class(lbl), 'News: '),
    span(id(dyknow), a(href=Link, Title))
  ]).

%! '_random_news'(-Id:atom, -Title:atom) is det.

'_random_news'(Id, Title):-
  aggregate_all(
    sum(Relevance),
    (
      post(Id, kind, news),
      relevance(Id, Relevance)
    ),
    SummedRelevance
  ),
  random_betwixt(SummedRelevance, R),
  find_posts(news, fresh, Ids),
  '_random_news'(0.0, R, Ids, Id, Title).
'_random_news'(_V, _R, [Id], Id, Title):- !,
  post(Id, title, Title).
'_random_news'(V1, R, [Id|_], Id, Title):-
  relevance(Id, Relevance),
  V2 is V1 + Relevance,
  R =< V2, !,
  post(Id, title, Title).
'_random_news'(V1, R, [Id0|Ids], Id, Title):-
  relevance(Id0, Relevance),
  V2 is V1 + Relevance,
  '_random_news'(V2, R, Ids, Id, Title).

request_to_id(Request, Id):-
  memberchk(path(Path), Request),
  atom_concat('/news/', Id, Path).

request_to_resource(Request, URL):-
  request_to_id(Request, Id),
  Id \== '',
  http_absolute_uri(post(Id), URL).

