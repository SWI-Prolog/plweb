:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).		% new

:- use_module(library(semweb/rdf_db)).
:- use_module(lod).

:- http_handler(root(.),	home,	  []).
:- http_handler(root(search),	search,	  []).
:- http_handler(root(resource),	resource, []).
:- http_handler(css('lod.css'),	http_reply_file('lod.css', []), []).

http:location(css, root(css), []).

:- html_resource(lod,
		 [ requires([ css('lod.css')
			    ]),
		   virtual(true)
		 ]).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

home(_Request) :-
	reply_html_page(title('LOD Crawler'),
			[ \html_requires(lod),
			  h1(class(title), 'LOD Crawler'),
			  p(class(banner),
			    [ 'Welcome to the SWI-Prolog Linked Open Data ',
			      'crawler.  To start your experience, enter a ',
			      'search term such as "Amsterdam".'
			    ]),
			  \search_form
			]).

search_form -->
	{ http_link_to_id(search, [], Ref) },
	html(form([id(search), action(Ref)],
		  [ input(name(q)),
		    input([type(submit), value('Search')])
		  ])).


search(Request) :-
	http_parameters(Request, [q(Query, [])]),
	sindice_query(Query, 1, URL),
	lod_load(URL),			% work around Sindice bug
	(   rdf(URI, _, _, URL)
	->  resource_page(URI)
	).

resource(Request) :-
	http_parameters(Request, [r(URI, [])]),
	catch(lod_load(URI), E, true),
	(   var(E)
	->  resource_page(URI)
	;   subsumes_chk(error(domain_error(content_type, _), _), E)
	->  throw(http_reply(moved_temporary(URI)))
	;   throw(E)
	).


resource_page(URL) :-
	uri_label(URL, Label),
	findall(P-O, rdf(URL, P, O), Pairs0),
	sort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, Grouped),
	reply_html_page(title('LOD Crawler --- ~w'-[Label]),
			[ h1(class(title), a(href(URL), Label)),
			  \property_table(Grouped)
			]).


property_table(Grouped) -->
	html([ \html_requires(lod),
	       table(class(properties),
		     [ \ptable_header
		     | \ptable_rows(Grouped)
		     ])
	     ]).

ptable_header -->
	html(tr([th('Predicate'), th('Object')])).

ptable_rows(Grouped) -->
	ptable_rows(Grouped, 1).

ptable_rows([], _) -->
	[].
ptable_rows([H|T], Row) -->
	{ Row1 is Row + 1 },
	ptable_row(H, Row),
	ptable_rows(T, Row1).

ptable_row(P-VL, Row) -->
	{ ( Row mod 2 =:= 0 -> Class = even ; Class = odd ) },
	html(tr(class(Class),
		[ td(class(predicate), \rlink(P)),
		  td(class(object),    ul(\vlist(VL)))
		])).

vlist([]) --> [].
vlist([H|T]) --> html(li(\vdiv(H))), vlist(T).

vdiv(literal(L)) --> !,
	{ text_of_literal(L, Text) },
	html(div(class(lvalue), Text)).
vdiv(R) -->
	html(div(class(rvalue), \rlink(R))).

rlink(P) -->
	{ uri_label(P, Label),
	  uri_iri(URI, P),
	  http_link_to_id(resource, [r=URI], HREF)
	},
	html(a(href(HREF), Label)).

%%	body(+Content)//
%
%	Define overall style

body(Content) -->
	html([ div(class(top), \search_form)
	     | Content
	     ]).
