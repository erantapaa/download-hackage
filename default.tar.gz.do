#!/bin/sh
#
# https://hackage.haskell.org/package/heredoc-0.2.0.0/heredoc-0.2.0.0.tar.gz

# ( echo  '$1 is: ' "$1," '$2 is: ' "$2") >&2

pkgtar=$(basename "$1")
pkg=$(basename "$2")

curl -L -s -S "https://hackage.haskell.org/package/$pkg/$pkgtar"

