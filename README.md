# The SWI-Prolog web-site

This repository contains the software  of http://www.swi-prolog.org. The
(wiki) content of the website is stored in a git submodule. This must be
installed seperately using the command below.   To install the site from
scratch locally, perform the following commands:

  1. Downloading the site

    ==
    % git clone git://www.swi-prolog.org/home/pl/git/plweb.git
    % cd plweb
    % git submodule update --init
    ==

  2. Install required SWI-Prolog add-ons:

    ==
    % swipl
    ?- pack_install(recaptcha).
    ?- pack_install(smtp).
    ==

## Running the site

After installation, the  website  may  be   started  locally  using  the
commands below. After that, you  have  access   to  the  same content as
available  from  http://www.swi-prolog.org,  except   for  the  download
section of the website. The  default  port   of  the  site  is 3040, and
therefore it may be accessed on http://localhost:3040/

  ==
  % swipl -s load.pl
  ?- server.
  ==

## Issues with the locally running site

  - There is no download section (but that can't be a big issue)

  - If you want to use the _login_ facility to play with the
    interactive aspects of the site, you need to

      1. Get a reCAPTCHA key-pair from Google

      2. Run (from a started server)

         ==
	 ?- set_setting(recaptcha:public_key, 'public key goes here').
	 ?- set_setting(recaptcha:private_key, 'private key goes here').
	 ?- save_settings.
	 ==

      3. Run the server from a port that is accessible from the public
         internet.

      4. Use an OpenID provider that is not too picky for your site.
         In our experience, Google is less picky than Yahoo.
