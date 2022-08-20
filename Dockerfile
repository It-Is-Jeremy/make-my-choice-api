FROM haskell:latest

WORKDIR /app
ADD . /app


RUN stack setup
RUN stack install
RUN stack build

EXPOSE 8080