#!/bin/bash

cabal clean
cabal sandbox delete
cabal sandbox init

cabal install --dependencies-only
cabal configure
cabal build

version=`cat keystone.cabal | grep "^version:" | awk --field-separator ' ' '{print $2;}'`
release=keystone-$version
mkdir $release
cp dist/build/keystone/keystone $release
cp policy.json $release
cp keystone.conf $release
cp README.md $release
cp CHANGELOG.md $release
cp LICENSE $release
cp server.crt $release
cp server.key $release
cp server.csr $release
tar zcvf ${release}.tar.gz $release
