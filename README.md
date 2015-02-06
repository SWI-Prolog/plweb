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
    ?- pack_install(googleclient).
    ?- pack_install(smtp).
    ==

  3. For a full installation, install the dynamic data.  The .db
    files must be writeable by the server process.

    - annotations.db
    Comments on web pages
    - tags.db
    Tags on web pages
    - openid.db
    User administration
    - packs.db
    Known packages
    - post.db
    News posts
    - reviews.db
    Pack reviews
    - download
    Points to the download directory

    Install the download descriptions by running the script
    `install-custom`

  4. Create directories for logging and pack mirrors.  These
     directories must be writeable by the server and new directories
     created below must have the same permissions:

    ==
    % mkdir log pack
    % chgrp www-data log pack
    % chmod g+ws log pack
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

### Running as daemon using Ubuntu upstart

A good way to run the website on a   Linux server is by creating a Linux
container using lxc. After installing the server,   you can enable it to
start at boot time by   copying `upstart/swi-prolog.conf` to `/etc/init`
after editing it to suit your   configuration  requirements. By default,
the server runs as user `www-data`, group `www-data` as specified in the
above configuration file.

Make sure the following components are  writeable to the server process.
For files, this means mode 664,   group  www-data. For directories, this
means mode 2775, group www-data.

  - log
  Write httpd.log and pack-warnings.log

  - pack
  Mirrors known packages.  Will be populated as the server is started.

  - www: subdirectories and .txt files
  Needs to make the wiki pages editable.  It is also wise to do this in
  a git branch.  From the www directory, do:

    ==
    % git checkout master
    % git pull
    % git checkout -b wiki
    % find . -type d | xargs chmod 2775
    % find . -name '*.txt' | xargs chmod 664
    % chgrp -W www-data .
    ==

  - *.db


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
