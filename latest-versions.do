#!/bin/sh

redo-ifchange index.tar.gz
redo-ifchange parse-versions
tar tf index.tar.gz | ./parse-versions

