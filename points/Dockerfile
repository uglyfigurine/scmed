FROM haskell:latest

#COPY ghcup.sh /usr/bin/ghcup.sh
RUN mkdir -p /opt/points
WORKDIR /opt/points
COPY app/ /opt/points/app
COPY src/ /opt/points/src
COPY points.cabal /opt/points/points.cabal
#COPY LICENSE /opt/points/LICENSE
COPY Setup.hs /opt/points/Setup.hs
RUN apt update
RUN apt-get install -y libglib2.0-dev
RUN apt-get install -y libgirepository1.0-dev
RUN apt-get install -y libgtk-3-dev
RUN cabal new-update
RUN cabal new-build

CMD ["cabal", "new-run"]
