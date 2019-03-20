FROM practicalscheme/gauche

RUN apt-get update
RUN apt-get install -y libpq-dev libssl-dev
RUN apt-get install -y autoconf git

WORKDIR /build
RUN git clone https://github.com/kahua/Gauche-dbd-pg.git
WORKDIR /build/Gauche-dbd-pg
RUN ./DIST gen
RUN ./configure
RUN make
RUN make install

RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g nodemon

WORKDIR /code
CMD make run
