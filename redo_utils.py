import subprocess
import os
import sys
import re

def load_lines(path):
  with open(path) as f:
    for x in f:
      x = x.strip()
      if len(x): yield x

def batched(size,it):
  batch = []
  while True:
    x = next(it, None)
    if x == None:
      if len(batch): yield batch
      break
    batch.append(x)
    if len(batch) >= size:
      yield batch
      batch = []

def latest_versions():
  """Return an iterator of all latest-versions
     omitting the items listed in exclude-list."""
  excluded = set( list(load_lines("exclude-list")) )
  for x in load_lines("latest-versions"):
    if not x in excluded:
      yield x

def redo_ifchange(args):
  """Run redo-ifchange. Exit program if not successful.""" 
  cmd = ["redo-ifchange"]
  cmd.extend(args)
  status = subprocess.call(cmd)
  if status != 0:
    sys.exit(1)

