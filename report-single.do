#!/bin/sh

redo-ifchange not-ok-files
redo-ifchange lex-test-exe
./lex-test-exe singleQuotes < ./not-ok-files

