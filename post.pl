:- module(
  post,
  [
    find_posts/3, % +Kind:oneof([annotation,news])
                  % :CheckId
                  % -Ids:list(atom)
    fresh/1, % ?Id:atom
    all/1,% ?Id:atom
    post/3, % ?Post:or([atom,compound])
            % ?Name:atom
            % ?Value
    post//3, % +Options:list(nvpair)
             % +About:atom
             % +Post:compound
    posts//3, % +Kind:oneof([annotation,news])
              % +About:atom
              % +Ids:list(atom)
    relevance/2, % +Id:atom
                 % -Relevance:between(0.0,1.0))
    post_process/2, % +Request:list
                    % +Id:atom
    sort_posts/2 % +Ids:list(atom)
                 % -SortedIds:list(atom)
  ]
).

/** <module> Posts

@author Wouter Beek
@tbd Type-based JS response.
     After DELETE: remove that post from DOM.
     After POST: add that post to DOM.
     After PUT: update that post in DOM.
@tbd Showing the markItUp DOM does not work for the edit post link.
@version 2013/12-2014/01
*/

:- use_module(generics).
:- use_module(library(crypt)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/js_write)).
:- use_module(library(http/json_convert)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(persistency)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(uri)).
:- use_module(openid).

:- meta_predicate(find_posts(+,1,-)).

% /css
:- html_resource(css('post.css'), []).

% /js
:- html_resource(js('markitup/sets/pldoc/set.js'), [
  requires([
    js('markitup/jquery.markitup.js'),
    js('markitup/skins/markitup/style.css'),
    js('markitup/sets/pldoc/style.css')
  ])
]).

:- dynamic(category/1).

:- persistent(post(id:atom,post:compound)).

:- initialization(db_attach('post.db', [sync(close)])).

:- json_object post(
  kind:oneof([annotation,news]),
  title,
  content:atom,
  meta:meta/6
) + [type=post].
:- json_object meta(
  author:atom,
  about,
  categories:list(atom),
  importance,
  time:time/2,
  votes:votes/2
) + [type=meta].
:- json_object votes(
  down:nonneg,
  up:nonneg
) + [type=votes].
:- json_object time(
  posted:nonneg,
  'freshness-lifetime'
) + [type=time].

% /category
http:location(category, root(category), []).
:- http_handler(root(category), category_overview, [prefix]).

% Image files can be loaded from the `/img` path.
http:location(img, root(img), []).
user:file_search_path(img, document_root(img)).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

http:location(post, root(post), []).
:- http_handler(post(.), post_process, [prefix]).



% PERSISTENCY PREDICATES %

assert_post(JSON):-
  json_to_prolog(JSON, Post),
  variant_sha1(Post, Id),
  assert_post(Id, Post).

retract_post(Id):-
  retract_post(Id, _).



% RESTFUL PREDICATES %

post_process(Request):-
  request_to_id(Request, post, Id),
  post_process(Request, Id).

post_process(Request, Id):-
  memberchk(method(Method), Request),
  (   site_user_logged_in(User)
  ->  true
  ;   User = anonymous
  ),
  post_process(Method, Request, Id, User).

% DELETE
post_process(delete, Request, Id, User):-
  % Make sure the user is the author.
  post(Id, author, User), !,
  retract_post(Id),
  http_response(Request, 204).
post_process(delete, Request, _, _):-
  http_response(Request, 401).

% GET
post_process(get, _, Id, _):-
  post(Id, Post), !,
  prolog_to_json(Post, JSON),
  reply_json(JSON).
post_process(get, Request, _, _):-
  http_response(Request, 404).

% POST
post_process(post, Request, _, _):-
  http_read_json(Request, JSON), !,
  assert_post(JSON),
  % @tbd If a resource has been created on the origin server, the response
  % SHOULD be 201 (Created) and contain an entity which describes the
  % status of the request and refers to the new resource, and a Location
  % header (see section 14.30).
  http_response(Request, 201).
post_process(post, Request, _, _):-
  http_response(Request, 400).

% PUT
post_process(put, Request, Id, User):-
  % Make sure the user is the author.
  post(Id, author, User), !,
  retract_post(Id),
  http_read_json(Request, JSON),
  assert_post(JSON),
  http_response(Request, 204).
post_process(put, Request, _, _):-
  http_response(Request, 401).



% ACCESSOR PREDICATES %

%! post(+Post:or([atom,compound]), +Name:atom, -Value) is semidet.
%! post(+Post:or([atom,compound]), +Name:atom, -Value) is det.

post(Post, Name, Value):-
  maplist(nonvar, [Post,Name]), !,
  post1(Post, Name, Value),
  Value \== @(null), !.
post(Post, Name, Value):-
  post1(Post, Name, Value).

post1(Post, about, About):-
  post1(Post, meta, meta(_,About,_,_,_,_)).
post1(Post, author, Author):-
  post1(Post, meta, meta(Author,_,_,_,_,_)).
post1(Post, categories, Categories):-
  post1(Post, meta, meta(_,_,Categories,_,_,_)).
post1(post(_,_,Content,_), content, Content).
post1(Post, 'freshness-lifetime', FreshnessLifetime):-
  post1(Post, time, time(_,FreshnessLifetime)).
post1(Post, id, Id):-
  post(Id, Post).
post1(Post, importance, Importance):-
  post1(Post, meta, meta(_,_,_,Importance,_,_)).
post1(post(Kind,_,_,_), kind, Kind).
post1(post(_,_,_,Meta), meta, Meta).
post1(Post, posted, Posted):-
  post1(Post, time, time(Posted,_)).
post1(Post, time, Time):-
  post1(Post, meta, meta(_,_,_,_,Time,_)).
post1(post(_,Title,_,_), title, Title).
post1(Id, votes, Votes):-
  post(Id, votes_down, Down),
  post(Id, votes_up, Up),
  Votes is Up - Down.
post1(Post, votes_down, Down):-
  post1(Post, votes_pair, votes(Down, _)).
post1(Post, votes_pair, Votes):-
  post1(Post, meta, meta(_,_,_,_,_,Votes)).
post1(Post, votes_up, Up):-
  post1(Post, votes_pair, votes(_, Up)).
post1(Id, Name, Value):-
  post(Id, Post),
  post1(Post, Name, Value).



% SINGLE POST %

%! post(+Options:list(nvpair), +About:atom, +Id:atom)// is det.
% Generates a post, based on the given compound term.
%
% The following options are supported (first parameter of `Post`):
%   * `orientation(+Orientation:oneof([left,right]))`
%     Orientation of the post.
%     This is used in binary conversations
%      to show the different conversation parties.
%   * `standalone(+Standalone:boolean)`
%     Whether this post is part of multiple posts or not.

post(O1, About, Id) -->
  % Extract post kind.
  {post(Id, kind, Kind)},

  % Orientation option.
  {
    option(orientation(Orient), O1, left),
    format(atom(Style), '"float:~a;"', [Orient])
  },

  html(
    article([class=[post,Kind],id=Id,style=Style], [
      \post_header(O1, Id),
      \post_section(Id),
      \edit_remove_post(Id)
    ])
  ),

  % Standalone option.
  (
    {option(standalone(true), O1, true)}
  ->
    html_requires(css('post.css')),
    (
      {site_user_logged_in(_)}
    ->
      html(\write_post_js(Kind, About))
    ;
      login_post(Kind)
    )
  ;
    []
  ).

post_categories(Id) -->
  {
    post(Id, categories, Categories),
    nonvar(Categories)
  }, !,
  post_categories1(Categories).
post_categories(_) --> [].

post_categories1([]) --> !, [].
post_categories1(L) -->
  html([' under ',span(class=categories, \post_categories2(L))]).

post_categories2([]) --> !, [].
post_categories2([H|T]) -->
  post_category(H),
  post_categories2(T).

post_category(Category) -->
  {
    category_exists(Category),
    http_absolute_location(category(Category), Link, [])
  },
  html(a([class='post-category',href=Link],Category)).

%! post_header(+Options:list(nvpair), +Id:atom)// is det.
% When the post appears in isolation (option =|standalone(true)|=),
%  the title is not displayed.

post_header(O1, Id) -->
  html(
    header([], [
      \post_title(O1, Id),
      \post_metadata(Id),
      \html_receive(edit_remove(Id)),
      \post_votes(Id)
    ])
  ).

post_metadata(Id) -->
  {post(Id, kind, Kind)},
  post_metadata(Kind, Id).

post_metadata(annotation, Id) -->
  {post(Id, author, Author)},
  html(
    span(class='post-meta', [
      \user_profile_link(Author),
      ' said (',
      \post_time(Id),
      '):'
    ])
  ).
post_metadata(news, Id) -->
  {post(Id, author, Author)},
  html(
    span(class='post-meta', [
      'By ',
      \user_profile_link(Author),
      %\post_categories(Id),
      ' at ',
      \post_time(Id)
    ])
  ).

post_section(Id) -->
  {
    post(Id, author, Author),
    post(Id, content, Content),
    atom_codes(Content, Codes),
    wiki_file_codes_to_dom(Codes, /, DOM1),
    clean_dom(DOM1, DOM2)
  },
  html(
    section([], [
      \author_image(Author),
      div(class='read-post', DOM2)
    ])
  ).

post_time(Id) -->
  {post(Id, posted, Posted)},
  html(\dateTime(Posted)).

post_title(O1, Id) -->
  {
    option(standalone(false), O1, true),
    post(Id, title, Title),
    nonvar(Title), !,
    post(Id, kind, Kind),
    Spec =.. [Kind,Id],
    http_absolute_location(Spec, Link, [])
  },
  html(h2(class='post-title',a(href=Link,Title))).
post_title(_, _) --> [].

post_votes(Id) -->
  {
    post(Id, votes_down, Down),
    format(atom(AltDown), '~d downvotes', [Down]),
    post(Id, votes_up, Up),
    format(atom(AltUp), '~d upvotes', [Up]),
    post(Id, votes, Amount)
  },
  html(
    span(class='post-vote', [
      a([class='post-vote-up',href=''],
        img([alt=AltUp,src='/icons/vote_up.gif',title=Up], [])
      ),
      ' ',
      span(class='post-vote-amount', Amount),
      ' ',
      a([class='post-vote-down',href=''],
        img([alt=AltDown,src='/icons/vote_down.gif',title=Down], [])
      )
    ])
  ).



% MULTIPLE POSTS %

posts(Kind, About, Ids1) -->
  {
    atomic_list_concat([Kind,component], '-', Class),
    sort_posts(Ids1, votes, Ids2)
  },
  html([
    \html_requires(css('post.css')),
    div(class=[posts,Class], \posts(Kind, About, left, Ids2)),
    \add_post(Kind, About)
  ]).

posts(_Kind, _About, _Orient, []) --> [].
posts(Kind, About, Orient1, [Id|Ids]) -->
  post([orientation(Orient1),standalone(false)], About, Id),
  {switch_orientation(Orient1, Orient2)},
  posts(Kind, About, Orient2, Ids).

switch_orientation(left, right):- !.
switch_orientation(right, left):- !.



% ADD POST %

add_post(Kind, About) -->
  {site_user_logged_in(_)}, !,
  html(
    div(id='add-post', [
      \add_post_link,
      form([id='add-post-content',style='display:none;'], [
        \add_post_title(Kind),
        \add_post_importance(Kind),
        \add_post_freshnesslifetime(Kind),
        \add_post_content
      ]),
      \submit_post_links,
      \write_post_js(Kind, About)
    ])
  ).
add_post(Kind, _) -->
  login_post(Kind).

add_post_content -->
  html(textarea([class=markItUp], [])).

add_post_freshnesslifetime(news) --> !,
  % Freshness lifetime is represented in seconds.
  % 1 day is appoximately 86.400 seconds.
  html([
    label([], 'Freshness lifetime: '),
    select(class='freshness-lifetime', [
      option(value=31536000, 'Very long'),                % One year (almost 'sticky')
      option(value=2678400, 'Long'),                      % One month
      option([selected=selected,value=604800], 'Normal'), % One week
      option(value=86400, 'Short'),                       % One day
      option(value=3600, 'Very short')                    % One hour
    ]),
    br([])
  ]).
add_post_freshnesslifetime(_) --> [].

add_post_importance(news) --> !,
  html([
    label([], 'Importance: '),
    select(class=importance, [
      option(value=1.00, 'Very high'),
      option(value=0.75, 'High'),
      option([selected=selected,value=0.50], 'Normal'),
      option(value=0.25, 'Low'),
      option(value=0.00, 'Very low')
    ])
  ]).
add_post_importance(_) --> [].

add_post_link -->
  html(a([id='add-post-link',href=''], 'Add post')).

add_post_title(news) --> !,
  html([
    label([], 'Title: '),
    input([class=title,size=70,type=text], []),
    br([])
  ]).
add_post_title(_) --> [].

submit_post_links -->
  html(
    div([id='add-post-links',style='display:none;'], [
      a(id='add-post-submit', 'Add comment'),
      a(id='add-post-cancel', 'Cancel')
    ])
  ).



% EDIT/REMOVE POST %

edit_post(Id) -->
  {
    post(Id, author, Author),
    site_user_logged_in(Author), !,
    post(Id, kind, Kind)
  },
  html([
    form([class='edit-post-content',style='display:none;'], [
      % @tbd Set title value.
      \add_post_title(Kind),
      % @tbd Set importance value.
      \add_post_importance(Kind),
      % @tbd Set freshness lifetime value.
      \add_post_freshnesslifetime(Kind),
      \edit_post_content(Id)
    ]),
    \save_post_links
  ]).
edit_post(_) --> [].

edit_post_content(Id) -->
  {post(Id, content, Content)},
  html(textarea([class=markItUp,cols=80,style='display:none;'], Content)).

edit_remove_post(Id) -->
  {
    post(Id, author, Author),
    site_user_logged_in(Author), !
  },
  html([
    \html_post(edit_remove(Id), \edit_remove_post_link),
    \edit_post(Id)
  ]).
edit_remove_post(_) --> [].

edit_remove_post_link -->
  html(
    div(class='post-links', [
      a([class='edit-post-link',href=''], 'Edit'),
      '/',
      a([class='remove-post-link',href=''], 'Delete')
    ])
  ).

save_post_links -->
  html(
    div([class='edit-post-links',style='display:none;'], [
      a([class='edit-post-submit',href=''], 'Edit comment'),
      a([class='edit-post-cancel',href=''], 'Cancel')
    ])
  ).



% HELPERS %

%! age(+Id:atom, -Age:nonneg) is det.

age(Id, Age):-
  post(Id, posted, Posted),
  get_time(Now),
  Age is Now - Posted.

%! author_image(+User:atom)// is det.

author_image(User) -->
  {
    site_user_property(User, name(Name)),
    format(atom(Alt), 'Picture of user ~w.', [Name]),

    % @see Documentation
    %      https://en.gravatar.com/site/implement/hash/
    site_user_property(User, email(Email)),
    strip_spaces(Email, Temp1),
    downcase_atom(Temp1, Temp2),
    md5(Temp2, Hash),

    % @see Documentation
    %      https://en.gravatar.com/site/implement/images/
    % @see Example
    %      http://www.gravatar.com/avatar/205e460b479e2e5b48aec07710c08d50
    uri_path([avatar,Hash], Path),
    uri_components(URL, uri_components(http, 'www.gravatar.com', Path, _, _)),

    % Also provide a link to the user profile.
    http_link_to_id(view_profile, [user(User)], Link)
  },
  html(a(href=Link,img([alt=Alt,class='post-avatar',src=URL,title=Name]))).

category_exists(Category):-
  category(Category), !.
category_exists(Category):-
  assert(category(Category)).

category_overview(Request):-
  memberchk(path(Path), Request),
  atom_concat('/category/', Category, Path),
  once(category(Category)), !,
  format(atom(Title), 'Category -- ~a', [Category]),
  reply_html_page(wiki, title(Title), ['Category ',Category,' TODO']).

dateTime(TimeStamp) -->
  % Alternative format: `%A %d %B %Y`.
  {format_time(atom(Date), '%Y-%m-%dT%H:%M:%S', TimeStamp)},
  html(span([class(date),title(TimeStamp)], Date)).

%! find_posts(
%!  +Kind:oneof([annotation,news]),
%!  :CheckId,
%!  -Ids:list(atom)
%! ) is det.

find_posts(Kind, CheckId, Ids):-
  findall(
    Id,
    (
      post(Id, Post),
      post(Post, kind, Kind),
      call(CheckId, Id)
    ),
    Ids
  ).

%! fresh(+Id:atom) is semidet.
%! fresh(-Id:atom) is nondet

fresh(Id):-
  post(Id, 'freshness-lifetime', FreshnessLifetime),
  nonvar(FreshnessLifetime), !,
  age(Id, Age),
  Age < FreshnessLifetime.
fresh(_).

%! all(+Id:atom) is det.
%
%  News filter, retuning all objects

all(_).

%! relevance(+Id:atom, -Relevance:between(0.0,1.0)) is det.
% - If `Importance` is higher, then the dropoff of `Relevance` is flatter.
% - `Relevance` is 0.0 if `FreshnessLifetime =< Age`.
% - `Relevance` is 1.0 if `Age == 0`.

relevance(Id, Relevance):-
  fresh(Id),
  post(Id, importance, Importance),
  nonvar(Importance),
  post(Id, 'freshness-lifetime', FreshnessLifetime),
  nonvar(FreshnessLifetime), !,
  age(Id, Age),
  Relevance is Importance * (1 - Age / FreshnessLifetime).
relevance(_, 0.0).

sort_posts(Ids, SortedIds):-
  sort_posts(Ids, posted, SortedIds).

sort_posts(Ids, Property, SortedIds):-
  findall(
    Value-Id,
    (
      member(Id, Ids),
      post(Id, Property, Value)
    ),
    Temp1
  ),
  keysort(Temp1, Temp2),
  reverse(Temp2, Temp3),
  pairs_values(Temp3, SortedIds).

%! stale(+Id:atom) is semidet.
%! stale(?Id:atom) is nondet

stale(Id):-
  \+ fresh(Id).



% SCRIPT %

login_post(Kind) -->
  html(div(class='post-login', [b([],\login_link),' to add a new ',Kind,' post.'])).

% We assume the user is logged in once we get here.
write_post_js(Kind, About) -->
  {
    site_user_logged_in(Author),
    Spec =.. [Kind,'.'],
    http_absolute_uri(Spec, URL)
  },
  html([
    \html_requires(js('markitup/sets/pldoc/set.js')),
    \js_script({|javascript(Kind,URL,Author,About)||
      function http_post(url, updatePosted, form, method) {
        var down = parseInt(form.siblings("header").find(".post-vote-down").children("img").attr("title"));
        if (isNaN(down)) {down = 0;}

        var up = parseInt(form.siblings("header").find(".post-vote-up").children("img").attr("title"));
        if (isNaN(up)) {up = 0;}

        var title1, title2;
        title1 = form.find(".title").val();
        if (title1 === undefined) { title2 = null; } else { title2 = title1; }

        var posted;
        if (updatePosted) {
          posted = Math.round(Date.now()/1000);
        } else {
          var header = form.siblings("header");
          var date = header.find(".date");
          var title = date.attr("title");
          posted = parseInt(title);
        }

        $.ajax(
          url,
          {
            "contentType": "application/json; charset=utf-8",
            "data": JSON.stringify({
              "content": form.find(".markItUpEditor").val(),
              "kind": Kind,
              "meta": {
                "author": Author,
                "about": About,
                "categories": [Kind],
                "importance": parseFloat(form.find(".importance").val()),
                "time": {
                  "freshness-lifetime": parseInt(form.find(".freshness-lifetime").val(), 10),
                  "posted": posted,
                  "type": "time"
                },
                "type": "meta",
                "votes": {
                  "down": down,
                  "type": "votes",
                  "up": up
                }
              },
              "title": title2,
              "type": "post"
            }),
            "dataType": "json",
            "success": function() {location.reload(true);},
            "type": method
          }
        );
      }
      $(document).ready(function() {
        // Clicking this removes the UI for entering a new post.
        $("#add-post-cancel").click(function(e) {
          e.preventDefault();
          $("#add-post-links").css("display","none");
          $("#add-post-content").css("display","none");
          $("#add-post-link").css("display","block");
        });

        // Clicking this brings up the UI for entering a new post.
        $("#add-post-link").click(function(e) {
          e.preventDefault();
          $(this).css("display","none");
          $("#add-post-content").css("display","block");
          $("#add-post-links").css("display","block");
        });

        // Clicking this submits the current content as a new post.
        $("#add-post-submit").click(function(e) {
          e.preventDefault();
          http_post(
            URL,
            true,
            $(this).parent().siblings("#add-post-content"),
            "POST"
          );
        });

        // Clicking this removes the UI for editing an existing post.
        $(".edit-post-cancel").click(function(e) {
          e.preventDefault();
          var article = $(this).closest("article");
          article.children(".edit-post-links").css("display","none");
          article.children("form").css("display","none");
          article.children("header").css("display","block");
          article.children("section").css("display","block");
        });

        // Clicking this brings up the UI for editing an existing post.
        // Notice that this first hides the readable content element,
        //  and then shows the writable content element.
        $(".edit-post-link").click(function(e) {
          e.preventDefault();
          var article = $(this).closest("article");
          article.children("header").css("display","none");
          article.children("section").css("display","none");
          article.children("form").css("display","block");
          article.children(".edit-post-links").css("display","block");
        });

        // Clicking this submits the current changes to a post.
        $(".edit-post-submit").click(function(e) {
          e.preventDefault();
          var id = $(this).closest(".post").attr("id");
          http_post(
            URL+id,
            true,
            $(this).closest(".edit-post-links").siblings(".edit-post-content"),
            "PUT"
          );
        });

        // Mark-it-up support for entering post content.
        $(".markItUp").markItUp(pldoc_settings);

        // Clicking this removes an existing post.
        $(".remove-post-link").click(function(e) {
          e.preventDefault();
          var id = $(this).parents(".post").attr("id");
          $.ajax(
            URL+id,
            {
              "contentType": "application/json; charset=utf-8",
              "dataType": "json",
              "success": function() {location.reload(true);},
              "type": "DELETE"
            }
          );
        });

        // Clicking this decreases the number of votes by one.
        $(".post-vote-down").click(function(e) {
          e.preventDefault();
          $(this).children("img").attr(
            "title",
            parseInt($(this).children("img").attr("title")) + 1
          );
          http_post(
            URL+$(this).parents(".post").attr("id"),
            false,
            $(this).closest("header").siblings("form"),
            "PUT"
          );
        });

        // Clicking this increases the number of votes by one.
        $(".post-vote-up").click(function(e) {
          e.preventDefault();
          $(this).children("img").attr(
            "title",
            parseInt($(this).children("img").attr("title")) + 1
          );
          http_post(
            URL+$(this).parents(".post").attr("id"),
            false,
            $(this).closest("header").siblings("form"),
            "PUT"
          );
        });
      });
    |})
  ]).

