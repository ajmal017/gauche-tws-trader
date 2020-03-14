FROM practicalscheme/gauche

# https://askubuntu.com/questions/939345/the-package-cache-file-is-corrupted-error
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get update
RUN apt-get install -y autoconf git

RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g nodemon

# Build the HEAD version of Gauche for the C++ compilation issue:
# https://github.com/shirok/Gauche/pull/560

WORKDIR /build
RUN git clone https://github.com/shirok/Gauche.git
WORKDIR Gauche
RUN ./DIST gen
RUN ./configure && make && make check && make install

####

WORKDIR /build
RUN git clone git://github.com/bizenn/Gauche-redis.git
WORKDIR Gauche-redis
RUN ./DIST gen
RUN ./configure
RUN make install

WORKDIR /code
CMD make run
