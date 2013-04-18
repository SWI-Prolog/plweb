:- module(tmp,
	  [ wiki_all/0
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(rating).
:- use_module(markitup).

:- http_handler(root(rate), rate, []).
:- http_handler(root(rated), rated, []).

rate(_Request) :-
	http_link_to_id(rated, [], OnRating),
	reply_html_page(
	    title(rate),
	    [ h1('Rate pack "hello"'),
	      table([ tr([ th('Rating:'),
			   td(\rate([ on_rating(OnRating),
				      rate_max(5),
				      type(small),
				      step(true),
				      data_id(refactor)
				    ]))
			 ]),
		      tr([ th('Comments:'),
			   td(\markitup([ markup(pldoc)
					]))
			 ])
		    ])
	    ]).

rated(Request) :-
	http_parameters(Request,
			[ action(Action, []),
			  rate(Rate, [number])
			], []),
	format('Content-type: text/plain\n\n'),
	format('Ok: ~w=~w\n', [Action, Rate]).


		 /*******************************
		 *	     NEW WIKI		*
		 *******************************/

:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_html)).

wiki_all :-
	findall(Obj, doc_comment(Obj, _Pos, _Summary, _Comment), Objects),
	phrase(objects(Objects, []), _HTML).

