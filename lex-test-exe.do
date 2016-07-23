#!/bin/sh

# (echo "1: $1"; echo "2: $2"; echo "3 is $3") >&2

copy_exe() {
  install_root=$(cd lex-test && stack path | sed -ne 's,^local-install-root: ,,p')
  if [ "$install_root" = "" ]; then
    echo bad install root >&2 ; exit 1
  fi
  path="$install_root/bin/lex-test"
  # echo "path is: $path" >&2
  if [ -f "$path" ]; then
    cp "$path" "$1"
    chmod a+rx "$1"
  else
    echo "lex-test not found at $path"
    exit 1
  fi
}

( cd lex-test && stack build) && copy_exe "$3"

