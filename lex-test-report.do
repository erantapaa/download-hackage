#!/bin/sh

redo-ifchange all-hs-files
redo-ifchange lex-test-exe
./lex-test-exe < ./all-hs-files

