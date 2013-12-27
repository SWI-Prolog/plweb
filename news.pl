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

:- use_module(library(aggregate)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(pairs)).
:- use_module(library(persistency)).
:- use_module(markitup).
:- use_module(openid).
:- use_module(wiki).

http:location(news, root(news), []).
:- http_handler(root(add_news), add_news, []).
:- http_handler(root(news), news, [prefix]).
:- http_handler(news(archive), news_archive, []).

% Image files can be loaded from the `/img` path.
http:location(img, root(img), []).
user:file_search_path(img, document_root(img)).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

%! news(
%!   ?Id:atom,
%!   ?Title:atom,
%!   ?Content:atom,
%!   ?Importance:between(0.0,1.0),
%!   ?Posted:nonneg,
%!   ?FreshnessLifetime:nonneg,
%!   ?User:atom
%! ) is nondet.
% Internal listing of news items.
%
% @param Id The atomic identifier of the news item.
% @param Title The atomic title for the news item.
% @param Content ...
% @param Importance The importance of the news item.
% @param Posted The time at which the news item was posted.
% @param FreshnessLifetime How long the news items stays news-worthy.
% @param User The atomic name of the user who posted the news item.

:- persistent(
  news(
    id:atom,
    title:atom,
    content:atom,
    importance:between(0.0,1.0),
    posted:nonneg,
    freshness_lifetime:nonneg,
    user:atom
  )
).

:- initialization(db_attach('news.db', [sync(close)])).



% READ NEWS

% A specific news item.
news(Request):-
  memberchk(path(Path), Request),
  atom_concat('/news/', Id, Path),
  once(news(Id, Title1, _, _, _, _, _)), !,
  atomic_list_concat(['News',Title1], ' -- ', Title2),
  reply_html_page(wiki, title(Title2), \news_item_body(Id)).
% The list of fresh news items.
news(_Request):-
  find_news(fresh, Ids),
  reply_html_page(wiki, title('News'), [h1('News items'),\news_body(Ids)]).

% The list of fresh and stale (i.e., all) news items.
news_archive(_Request):-
  find_news(all, Ids),
  reply_html_page(
    wiki,
    title('News archive'),
    [h1('Archived news items'),\news_body(Ids)]
  ).

news_body(Ids) -->
  html([
    \html_requires(css('news.css')),
    div(class=content, [
      hr([]),
      div(class=posts, \news_items(Ids)),
      \add_news_link
    ])
  ]).

news_items([]) --> [].
news_items([Id|Ids]) -->
  news_item(Id),
  html(hr([])),
  news_items(Ids).

news_item(Id) -->
  {
    once(news(Id, Title, Content, _, Posted, _, User)),
    http_absolute_uri(news(Id), Link),
    atom_codes(Content, Codes),
    wiki_file_codes_to_dom(Codes, /, DOM1),
    clean_dom(DOM1, DOM2),
    relevance(Id, Relevance)
  },
  html([
    section(class=post, [
      header(class='post-header', [
        \author_image(User),
        h2(class='post-title', a(href=Link,Title)),
        p(class='post-meta', [
          'By ',
          a(class='post-author', User),
          ' under ',
          a([class=['post-category','post-category-news'],href='#'],'News'),
          ' at ',
          \dateTime(Posted),
          ' (relevance:', Relevance, ')'
        ])
      ]),
      div(class='post-description', DOM2)
    ])
  ]).

author_image(User) -->
  {
    Size = 75,
    format(atom(UserAlt), 'Picture of user ~w.', [User]),
    file_name_extension(User, png, UserImage),
    http_absolute_uri(img(UserImage), UserImageLink)
  },
  html(
    img([alt=UserAlt,class='post-avatar',height=Size,src=UserImageLink,width=Size])
  ).

news_item_body(Id) -->
  html([
    \html_requires(css('news.css')),
    \news_backlink,
    \news_item(Id)
  ]).

news_backlink -->
  {http_absolute_uri(root(news), Link)},
  html(a(href=Link, 'Back to list of news items')).

add_news_link -->
  {http_link_to_id(add_news, [], AddNews)},
  html(p(a(href=AddNews, 'Add news'))).



% WRITE NEWS

% Store a new news story.
% @tbd Allow users to login using `site_user_logged_in/1`.
add_news(Request):-
  http_parameters(Request, [
    content(Content, [atom,default('')]),
    freshness_lifetime(FreshnessLifetime, [default(604800),nonneg]),
    importance(Importance, [between(0.0,1.0),default(0.50)]),
    title(Title, [atom,default('')])
  ]),

  % The content and title must be non-empty.
  \+ is_empty(Content),
  \+ is_empty(Title),

  % The same news item cannot be posted multiple times
  % (not even by different people).
  variant_sha1(Title-Content, Id),
  \+ news(Id, _, _, _, _, _, _), !,

  User = 'Wouter Beek', % @tbd

  % The time at which the news item was posted.
  get_time(Posted1),
  Posted2 is round(Posted1),

  assert_news(
    Id,
    Title,
    Content,
    Importance,
    Posted2,
    FreshnessLifetime,
    User
  ),

  % Show the updated list of news items.
  http_redirect(moved_temporary, root(news), Request).
% Show the fill-in form for a new news item.
add_news(_Request):-
  reply_html_page(wiki, \add_news_head, \add_news_body).

add_news_head -->
  html(title('Add news')).

add_news_body -->
  {
    %site_user_logged_in(_User), !,
    http_link_to_id(add_news, [], AddNews)
  },
  html(
    form(
      [action(AddNews),id(add_news_form),method('POST')],
      [
        % Title
        label([for=title], 'Title: '),
        input([id=title,name=title,size=70,type=text], []),
        br([]),

        % Importance
        label([for=importance], 'Importance: '),
        select([id=importance,form=add_news_form,name=importance], [
          option(value=1.00, 'Very high'),
          option(value=0.75, 'High'),
          option([selected=selected,value=0.50], 'Normal'),
          option(value=0.25, 'Low'),
          option(value=0.00, 'Very low')
        ]),

        % Freshness lifetime is represented in seconds.
        % 1 day is appoximately 86.400 seconds.
        label([for=freshness_lifetime], 'Freshness lifetime: '),
        select([id=freshness_lifetime,form=add_news_form,name=freshness_lifetime], [
          option(value=31536000, 'Very long'),                % One year (almost 'sticky')
          option(value=2678400, 'Long'),                      % One month
          option([selected=selected,value=604800], 'Normal'), % One week
          option(value=86400, 'Short'),                       % One day
          option(value=3600, 'Very short')                    % One hour
        ]),
        br([]),

        % Content
        table([
          tr(td(\markitup([id(content),markup(pldoc)]))),
          tr(td(align(right),input([type(submit),value('Save comment')])))
        ])
      ]
    )
  ).
add_news_body -->
  html(
    div(class('news-login'), [\login_link,' to add a news item.'])
  ).



% SUPPORT PREDICATES

%! age(+Id:atom, -Age:nonneg) is det.

age(Id, Age):-
  news(Id, _, _, _, Posted, _, _),
  get_time(Now),
  Age is Now - Posted.

% @tbd Occurs in multiple locations.
clean_dom([p(X)], X) :- !.
clean_dom(X, X).

dateTime(TimeStamp) -->
  {format_time(atom(Atom), '%Y-%m-%dT%H:%M:%S', TimeStamp)},
  html([Atom]).

%! is_empty(+Content:atom) is semidet.

is_empty(Content):-
  normalize_space(atom(''), Content).

%! find_news(+Status:oneof([all,fresh]), -Ids:list(atom)) is det.

find_news(Status, Ids):-
  findall(
    Posted-Id,
    (
      news(Id, _, _, _, Posted, _, _),
      (Status == fresh -> fresh(Id) ; true)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  pairs_values(Pairs3, Ids).

%! fresh(+Id:atom) is semidet.
%! fresh(?Id:atom) is nondet

fresh(Id):-
  news(Id, _, _, _, _, FreshnessLifetime, _),
  age(Id, Age),
  Age < FreshnessLifetime.

%! stale(+Id:atom) is semidet.
%! stale(?Id:atom) is nondet

stale(Id):-
  \+ fresh(Id).

% @tbd Move to openid module.
login_link -->
  {http_current_request(Request)},
  login_link(Request).

%! random_betwixt(+UpperLimit:number, -Random:float) is det.

random_betwixt(UpperLimit, Random):-
  integer(UpperLimit), !,
  random_betwixt(0, UpperLimit, Random).
random_betwixt(UpperLimit, Random):-
  float(UpperLimit), !,
  random_betwixt(0.0, UpperLimit, Random).

random_betwixt(LowerLimit, UpperLimit, Random):-
  Random is LowerLimit + random_float * (UpperLimit - LowerLimit).

random_news -->
  {
    '_random_news'(Id, Title),
    http_absolute_uri(news(Id), Link)
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
      news(Id, _, _, _, _, _, _),
      relevance(Id, Relevance)
    ),
    SummedRelevance
  ),
  random_betwixt(SummedRelevance, R),
  find_news(fresh, Ids),
  '_random_news'(0.0, R, Ids, Id, Title).

'_random_news'(_V, _R, [Id], Id, Title):- !,
  once(news(Id, Title, _, _, _, _, _)).
'_random_news'(V1, R, [Id|_], Id, Title):-
  relevance(Id, Relevance),
  V2 is V1 + Relevance,
  R =< V2, !,
  once(news(Id, Title, _, _, _, _, _)).
'_random_news'(V1, R, [Id0|Ids], Id, Title):-
  relevance(Id0, Relevance),
  V2 is V1 + Relevance,
  '_random_news'(V2, R, Ids, Id, Title).

%! relevance(+Id:atom, -Relevance:between(0.0,1.0)) is det.
% - If `Importance` is higher, then the dropoff of `Relevance` is flatter.
% - `Relevance` is 0.0 if `FreshnessLifetime =< Age`.
% - `Relevance` is 1.0 if `Age == 0`.

relevance(Id, 0.0):-
  stale(Id), !.
relevance(Id, Relevance):-
  once(news(Id, _, _, Importance, _, FreshnessLifetime, _)),
  age(Id, Age),
  Relevance is Importance * (1 - Age / FreshnessLifetime).

