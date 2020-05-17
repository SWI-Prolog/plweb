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

:- http_handler(root(doc_link), doc_link, []).

%!  doc_link(+Request)
%
%   Get a link to the documentation for a predicate reference.

doc_link(Request) :-
    http_parameters(Request,
                    [ for(For, [])
                    ]),
    cors_enable,
    (   for_link(For, Link)
    ->  reply_json_dict(Link)
    ;   reply_json_dict(null, [status(404)])
    ).

for_link(For, Info) :-
    atom_to_object(For, Obj),
    current_man_object(Obj),
    findall(Prop, obj_property(Obj, Prop), Props),
    dict_pairs(Info, json, Props).

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
