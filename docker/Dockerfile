FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
        git curl ca-certificates unzip \
        build-essential cmake autoconf ninja-build pkg-config \
        gdb \
        cleancss node-requirejs uglifyjs \
        ncurses-dev libreadline-dev libedit-dev \
        libgoogle-perftools-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre2-dev \
        libyaml-dev \
	libpython3-dev \
	default-jdk junit4 \
        libssh-dev openssh-client \
        locales

RUN     sed -i -e 's/# en_GB.UTF-8 UTF-8/en_GB.UTF-8 UTF-8/' /etc/locale.gen && \
        locale-gen
ENV     LC_ALL en_GB.UTF-8
ENV     LANG en_GB.UTF-8
ENV     LANGUAGE en_GB:en

# Primary build

ENV     REBUILD_MOST 4
RUN     mkdir -p /usr/local/src && cd /usr/local/src && \
        git clone --recursive https://github.com/SWI-Prolog/swipl-devel.git && \
        cd swipl-devel && mkdir build && cd build && \
        cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=PGO -G Ninja .. && \
        ninja && ninja install

ENV	REBUILD_PLWEB 5
RUN     mkdir -p /srv && cd /srv && \
	git clone https://github.com/SWI-Prolog/plweb.git && \
	cd /srv/plweb && \
	git submodule update --init packs/googleclient \
				    packs/recaptcha \
				    packs/smtp \
				    packs/libssh

RUN	cd /srv/plweb && \
	SWIPL_PACK_PATH=packs swipl -g pack_rebuild -t halt

RUN	cd /usr/local/src/swipl-devel && \
	git remote add stable https://github.com/SWI-Prolog/swipl.git && \
	git fetch stable

# Update. Run `make update-plweb` or `make update-swipl` to update
# the `ENV` command below and redo the relevant part of the build

ENV     SWIPL_VERSION Wed 31 Jan 2024 06:17:31 PM CET
RUN     git config --global pull.ff only
RUN     cd /usr/local/src/swipl-devel && (git pull || git pull) && \
	git fetch stable && \
        git submodule update --init && \
        find build -name '*.qlf' | xargs rm && \
        cd build && rm -rf home && cmake . && ninja && \
        ninja install
ENV     PLWEB_VERSION Sun Feb 11 10:09:26 AM CET 2024
RUN     cd /srv/plweb && (git pull || git pull) && \
        git submodule update

# Running

copy health.sh health.sh
HEALTHCHECK --interval=30s --timeout=2m --start-period=1m CMD /health.sh

COPY start-plweb.sh /srv/start-plweb.sh

ENV PLWEB_DATA /srv/plweb/data
ENV PLWEB_HOME /srv/plweb
VOLUME ${PLWEB_DATA}
WORKDIR ${PLWEB_HOME}

ENTRYPOINT ["/srv/start-plweb.sh"]
