:- module(
  footer,
  [
    footer//2, % +Arg:compound
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
:- use_module(library(http/js_write)).
:- use_module(openid).
:- use_module(tagit).
:- use_module(annotation).

:- html_resource(css('footer.css'), []).



'community-content'(Object) -->
  {var(Object)}, !,
  [].
'community-content'(Object) -->
  html(
    div(id='community-content', [
      \tagit_footer(Object, []),
      \annotation(Object)
    ])
  ).

footer(Object, ShowUser) -->
  html([
    \html_requires(css('footer.css')),
    div(class=footer, [
      \'community-content'(Object),
      \'footer-footer'(ShowUser)
    ])
  ]),
  balance_columns_script.

'footer-footer'(ShowUser) -->
  html(
    div(id=footer, [
      \show_user(ShowUser),
      \server_information
    ])
  ).

show_user(false) --> !.
show_user(_) --> current_user.

balance_columns_script -->
	js_script({|javascript||
		   $().ready(function()
	           { var $navtree = $(".navtree");
		     var $navcontent = $(".navcontent");
		     if ( $navtree.length > 0 && $navcontent.length > 0 )
		     { var $window = $(window).on("resize", function()
		       { var ch = $navcontent.height();
			 var nh = $navtree.height();
			 if ( nh > 400 && nh > ch + 200 )
			 { if ( ch < 300 ) ch = 300;
			   $navtree.height(ch);
			   $navtree.css('overflow-y', 'scroll');
			 }
		       }).trigger("resize")
		     }
		   });
		  |}).


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

