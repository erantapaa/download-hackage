#!/bin/sh

pkgvers=$(basename "$2")

# (echo unpacking "$pkgvers" ) >&2
(cd downloads; /bin/rm -rf "./$pkgvers"; tar xf "../archives/$pkgvers.tar.gz")
echo Done
