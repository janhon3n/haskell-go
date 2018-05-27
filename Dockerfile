FROM haskell:8

RUN apt-get update
RUN apt-get install -y xz-utils make netbase

RUN stack upgrade
COPY . /app
WORKDIR /app
RUN stack install

EXPOSE 8000
CMD ["haskell-go-exe"]
