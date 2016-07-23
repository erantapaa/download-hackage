#!/bin/sh

redo-ifchange all-downloads
redo-ifchange latest-versions

sed -e 's,^,archives/,' -e 's,$,.tar.gz,' latest-versions | xargs redo-ifchange

# deps=($(tsort < deps.dump | tail -100))
# 
# d1=("${deps[@]/#/done/}")
# d2="${d1[@]/%/.pkg}"
# 
# redo-ifchange $d2
