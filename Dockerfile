FROM haskell

RUN apt-get update
RUN apt-get install -y xz-utils make

RUN stack upgrade
COPY . /app
WORKDIR /app
RUN stack install

RUN mkdir /react-client
COPY ./react-client/build/* /react-client/*

EXPOSE 8000
CMD ["haskell-go-exe"]
