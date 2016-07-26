#!/bin/sh
#
# Create a list of the NOT OK files

redo-ifchange report-default
sed -n -e 's/^NOT OK: *//p' report-all

