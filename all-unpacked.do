#!/usr/bin/env python

import os
import sys
import re
import subprocess

dont_unpack = set( ['tslib-0.1.4', 'edit-lenses-demo-0.1', 'hermes-1.3.4.3' ] )

def debug(x):
  sys.stderr.write(x + "\n")

def batched(size,it):
  batch = []
  while True:
    x = next(it, None)
    if x == None:
      if len(batch): yield batch
      break
    x = x.strip()
    if x in dont_unpack: continue
    batch.append(x)
    if len(batch) >= size:
      yield batch
      batch = []

def redo_ifchange(args):
  cmd = ["redo-ifchange"]
  cmd.extend(args)
  # debug("cmd: " + str(cmd) + "\n")
  status = subprocess.call(cmd)
  return status == 0

def main():
  # redo_ifchange(["latest-versions"])
  # debug("--- asdasdasd")
  ok = True
  with open("latest-versions") as f:
    for batch in batched(100,f):
      args = [ "unpack/{}.unpack".format(x) for x in batch ]
      ok = redo_ifchange(args) and ok
  if ok:
    sys.exit(0)
  else:
    sys.exit(1)


main()

