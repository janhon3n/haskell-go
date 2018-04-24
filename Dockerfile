FROM haskell:8

WORKDIR /dockerr
ADD . /dockerr

RUN stack ghci