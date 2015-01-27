#! /bin/bash

getopts v: opt
version=$OPTARG
path=$PWD

if [ -d /tmp/label-generator ]; then
    rm -R /tmp/label-generator
fi

cabal configure --prefix=/usr
cabal build
cabal copy --destdir=/tmp/label-generator

cd /tmp/label-generator

tar -czf label-generator.tar.gz usr

fpm -s tar -t deb -n label-generator -v $version -m "<o_westphal@online.de>" -p $path \
    --license BSD3 --description "small application to print stuff on sticky labels" \
    -d ghc -d texlive-latex-base -d texlive-latex-extra -d texlive-fonts-recommended -d texlive-lang-german -d libc-dev-bin \
    label-generator.tar.gz

cd $path
