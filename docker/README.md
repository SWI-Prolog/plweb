# A SWISH (SWI-Prolog for SHaring) docker

This repository provides a [Docker](https://www.docker.com/) image for
the [public swish instance](https://swish.swi-prolog.org).  This image
is similar to the normal [SWISH](https://hub.docker.com/r/swipl/swish/)
Docker image.  However, it is based on the GIT version of Prolog and
contains several additional packages such as CHAT80 and Wordnet.

## Building the image

The image is built by running

    make image

## Running the image

> Needs to be updated

The image may be used in many  configurations, both controlled by docker
options and options to the  entry   point.  As basic operation typically
already requires publishing ports and setting up  a volume for the data,
we added a bash script `swish.sh` that automates the common scenarios by
providing `docker run` options from defaults  and provided options. When
called with `-n`, as in `./swish.sh -n option ...`, it merery prints the
docker command it will execute.  The following options are processed:

  - `--port=N` <br>
    Modify the `-p` option to `-p N:3050`.  Default is 3050.
  - `--data=dir` <br>
    Mount the given directory as data.  Default is the working
    directory.
  - `--with-R[=from]` <br>
    Add `--volumes-from from` where _from_ defaults to `rserve`
    to connect to an
    [R docker image](https://github.com/JanWielemaker/rserve-sandbox)
  - `-n` <br>
    Just print the docker command that will be executed.
  - `-d` <br>
    Detach from the terminal
  - `-it` <br>
    Pass on (interactive)

All remaining options are passed to the entry point of the image.

### Data

The docker image maintains its data (user programs and configuration) on
the _volume_ `/data`. This may be mapped   to a host directory using the
docker `-v` options (see also the   `--data=dir`  option of `swish.sh`).

Within the data directory, SWISH manages the following items:

  - **config-enabled** is a directory where the configuration is stored.
  If it doesn't exist it is created and filled with a default
  configuration that depends on the provided options.  The directory
  and its files have the same owner and group as the root of the managed
  volume.

  - **data** is the directory where all dynamic user data is maintained.
  If the directory exists, the SWISH server is started with the user and
  group of the directory.  If it doesn't exist the directory is created
  as owned by `daemon.daemon` and the server is started with these
  credentials.

  - **https** If the `--https` option is passed, it creates or reuses
  this directory and the files `server.crt` and `server.key`. The
  created certificate is _self-signed_.

  - **passwd**  If authenticated mode is enabled, this file maintains
  the users and password hashes.


### Network access

The container creates a server at port `3050`. By default this is an
HTTP server. If `run <docker options> swish --https` is used, an HTTPS
server is started.


### The entry point

The entry point of the containser is   `/entry.sh`,  a shell script that
initialises the data volume if needed and  starts the server. It accepts
the following options:

  - `--bash` <br>
  Instead of starting the server, start a bash shell.  Terminate after
  bash completes.

  - `--help` <br>
  Emit short help.


### Configuration

The SWISH configuration is controlled by   files in the `config-enabled`
directory. Several commands may be used   to  control the configuration.
These are executed as below:

  ```
  docker run -it swish option ...
  ```

Multiple configuration options may be passed to update multiple facets
of the configuration. Normally the image stops after updating the
configuration. If `--run` is added the entry point starts the server
after updating the configuration.

The options provided are:

  - `--list-config` <br>
  List installed and available configuration items.  If an item is
  installed, indicate whether it is modified.

  - `--auth=type` <br>
  Change the configured authentication scheme.  This is one of

    - `always` <br>
    Run in fully _authenticated_ mode, forcing the user to login
    and allowing to execute arbitrary commands.  When executed for
    the first time, docker must be run _interactively_ (`run -it`)
    to create the first user.  Additional users are created using

        `docker run -it swish --add-user`.

    - `social` <br>
    Use _social_ login.  By default enables optional http login,
    login using google and stackoverflow.  Both need to be further
    configured by editing `config-enabled/auth_google.pl` and
    `config-enabled/auth_stackoverflow.pl`

    - `anon` (or anonymous) <br>
    This is the initial default, providing fully anonymous login,
    executing only sandboxed Prolog queries.

  - `--add-config file ...` <br>
  Add one or more configuration files by copying them from the
  available configuration directory.

  - `--add-user` <br>
  Add a new user to the HTTP authentication.  Prompts for user name,
  email, group (unused, use `users`) and password.

  - `--https` <br>
  Create an HTTPS server.  This uses the certificate from the
  `https` directory (see above).  If no certificate exists, a
  self-signed certificate is created.  The details may be refined
  using `--CN=host`, `--O=organization` and `--C=country`

## Starting R

    docker pull swipl/rserve
    docker run -d --net=none --name=rserve --restart unless-stopped swipl/rserve

