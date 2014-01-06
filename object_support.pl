:- module(
  object_support,
  [
    object_label/2, % +Object:compound
                    % -Label:atom
    object_id/2 % ?Object:compound
                % ?Id:atom
  ]
).

/** <module> Object support

*/

:- use_module(wiki).

:- dynamic(object_id_cache/2).



%! object_id(+Object:compound, -Id:atom) is det.
%! object_id(-Object:compound, +Id:atom) is semidet.
% Manage identifiers for objects.

object_id(Object, Id) :-
  object_id_cache(Object, Id), !.
object_id(Object, Id) :-
  ground(Object),
  variant_sha1(Object, Id),
  assertz(object_id_cache(Object, Id)).


%! object_label(+Object:compound, -Label:atom) is det.

object_label(Name/Arity, Label) :- !,
  format(atom(Label), 'predicate ~w/~w', [Name, Arity]).
object_label(Name//Arity, Label) :- !,
  format(atom(Label), 'non-terminal ~w/~w', [Name, Arity]).
object_label(M:Name/Arity, Label) :- !,
  format(atom(Label), 'predicate ~w:~w/~w', [M, Name, Arity]).
object_label(M:Name//Arity, Label) :- !,
  format(atom(Label), 'non-terminal ~w:~w//~w', [M, Name, Arity]).
object_label(f(Name/Arity), Label) :- !,
  format(atom(Label), 'function ~w/~w', [Name, Arity]).
object_label(c(Function), Label) :- !,
  format(atom(Label), 'C API function ~w()', [Function]).
object_label(Module:module(_Title), Label) :-
  module_property(Module, file(File)), !,
  file_base_name(File, Base),
  format(atom(Label), 'module ~w', [Base]).
object_label(section(ID), Label) :-
  prolog:doc_object_summary(section(_Level, _No, ID, _File), _, _, Title), !,
  format(atom(Label), 'Section "~w"', [Title]).
object_label(wiki(Location), Label) :-
  wiki_page_title(Location, Title),
  format(atom(Label), 'Wiki page "~w"', [Title]).
object_label(Obj, Label) :-
  term_to_atom(Obj, Label).

