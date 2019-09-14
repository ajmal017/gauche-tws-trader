FROM practicalscheme/gauche

# https://askubuntu.com/questions/939345/the-package-cache-file-is-corrupted-error
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get update
RUN apt-get install -y autoconf git

RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g nodemon

WORKDIR /build
RUN git clone git://github.com/bizenn/Gauche-redis.git
WORKDIR Gauche-redis
RUN ./DIST gen
RUN ./configure
RUN make install

WORKDIR /code
CMD make run
