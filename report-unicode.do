#!/bin/sh

redo-ifchange not-ok-files
redo-ifchange lex-test-exe
./lex-test-exe unicodeSyntax < ./not-ok-files

