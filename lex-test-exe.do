#!/bin/sh

# (echo "1: $1"; echo "2: $2"; echo "3 is $3") >&2

(cd lex-test && stack build) && cp ./lex-test/.stack-work/install/x86_64-osx/lts-6.0/7.10.3/bin/lex-test "$3"
chmod a+rx "$3"

