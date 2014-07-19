#!/bin/sh
mkdir ../distribution 2>/dev/null
cp -r trac.app ../distribution
cp trac ../distribution/trac.app/Contents/MacOS/
