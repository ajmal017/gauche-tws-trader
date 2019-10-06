FROM ubuntu

RUN apt update
RUN apt install -y xvfb
RUN apt install -y x11vnc

RUN apt install -y curl
WORKDIR /work
RUN curl https://download2.interactivebrokers.com/installers/tws/latest-standalone/tws-latest-standalone-linux-x64.sh > tws.sh
RUN echo n | sh tws.sh

RUN apt install -y xorg

CMD sh starttws.sh
