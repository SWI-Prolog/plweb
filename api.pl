:- module(plweb_api,
          []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/man_index)).
:- use_module(library(pldoc/doc_util)).
:- use_module(library(prolog_code)).
:- use_module(library(prolog_source)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).

:- http_handler(root(doc_link), doc_link, []).

%!  doc_link(+Request)
%
%   Get a link to the documentation for a predicate reference.  This
%   hander may be used in two ways:
%
%      - GET
%        Adding a parameter `for` providing the predicate you want
%        to have information about.
%      - POST
%        Passing a JSON list of strings.  The reply is a JSON object
%        with for each string the corresponding reply object (`null`
%        if the predicate is not known).

doc_link(Request) :-
    memberchk(method(post), Request),
    !,
    http_read_json_dict(Request, Atoms,
                        [value_string_as(atom)]),
    cors_enable,
    must_be(list(atom), Atoms),
    for_links(Atoms, Pairs),
    dict_create(Dict, json, Pairs),
    reply_json_dict(Dict).
doc_link(Request) :-
    http_parameters(Request,
                    [ for(For, [])
                    ]),
    cors_enable,
    (   for_link(For, Link)
    ->  reply_json_dict(Link)
    ;   reply_json_dict(null, [status(404)])
    ).

for_links([], []).
for_links([H|T0], [H-I|T]) :-
    (   for_link(H, I)
    ->  true
    ;   I = null
    ),
    for_links(T0, T).

for_link(For, Info) :-
    atom_to_object(For, Obj),
    current_man_object(Obj),
    findall(Prop, obj_property(Obj, Prop), Props),
    format(string(URL),
           'https://www.swi-prolog.org/pldoc/doc_for?object=~w',
           [For]),
    dict_pairs(Info, json, [url-URL|Props]).

obj_property(Obj, summary-Summary) :-
    once(man_object_property(Obj, summary(Summary))).
obj_property(PI, Prop) :-
    pi_head(PI, Head0),
    (   Head0 = M:_
    ->  Head = Head0
    ;   Head = M:Head0
    ),
    (   M=system,
        predicate_property(Head, iso)
    ->  Prop = (class-iso)
    ;   M=system,
        predicate_property(Head, built_in)
    ->  Prop = (class-builtin)
    ;   predicate_property(Head, autoload(File))
    ->  (   Prop = (class-autoload)
        ;   library_prop(File, Prop)
        )
    ;   predicate_property(Head, file(File)),
        predicate_property(Head, exported)
    ->  (   Prop = (class-library)
        ;   library_prop(File, Prop)
        )
    ).

library_prop(File, library-LibS) :-
    file_name_extension(File, pl, LibFile),
    file_name_on_path(LibFile, library(Lib)),
    format(string(LibS), '~w', [Lib]).
