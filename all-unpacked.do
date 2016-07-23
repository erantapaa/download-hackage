#!/usr/bin/env python

from redo_utils import redo_ifchange, latest_versions

def main():
  redo_ifchange(["latest-versions"])
  args = [ "unpack/{}.unpack".format(x) for x in latest_versions() ]
  redo_ifchange(args)

main()

