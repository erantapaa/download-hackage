#!/bin/sh
#
# Report on all of the files

redo-ifchange all-hs-files
redo-ifchange lex-test-exe
./lex-test-exe < ./all-hs-files

