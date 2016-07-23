#!/bin/sh

pkgvers=$(basename "$2")

# (echo unpacking "$pkgvers" ) >&2
(cd downloads; tar xf "../archives/$pkgvers.tar.gz")
echo Done
