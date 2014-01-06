:- module(
  footer,
  [
    footer//1, % +Arg:compound
    server_information//0
  ]
).

/** <module> Footer

Footer for SWI-Prolog Web pages.

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(openid).
:- use_module(tagit).

:- html_resource(css('footer.css'), []).



'community-content'(Object) -->
  {var(Object)}, !,
  [].
'community-content'(Object) -->
  html(
    div(id='community-content', [
      \(tagit:tagit_footer(Object, [])),
      \annotation(Object)
    ])
  ).

footer(Arg) -->
  {Arg = object(Object)},
  html([
    \html_requires(css('footer.css')),
    div(class=footer, [
      \'community-content'(Object),
      \'footer-footer'(Arg)
    ])
  ]).

'footer-footer'(Arg) -->
  html(
    div(id=footer, [
      \current_user(Arg),
      \server_information
    ])
  ).

prolog_version(Version) :-
  current_prolog_flag(version_git, Version), !.
prolog_version(Version) :-
  current_prolog_flag(version_data, swi(Ma,Mi,Pa,_)),
  format(atom(Version), '~w.~w.~w', [Ma,Mi,Pa]).

%! server_information// is det.
% Emit server information.

server_information -->
  {prolog_version(Version)},
  html(
    a([id=powered,href='http://www.swi-prolog.org'], [
      'Powered by SWI-Prolog ',
      Version
    ])
  ).

