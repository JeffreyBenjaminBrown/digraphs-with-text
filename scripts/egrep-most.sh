#!/bin/bash

wordCharFromFile="$(head -1 scripts/wordCharFromFile.txt)"
  # thanks, Nick Hyde!
    # reading in that expression as a file lets you avoid lots of 
    # tricky unquoting

egrep "(^|$wordCharFromFile)$1($|$wordCharFromFile)" --exclude-dir=.stack-work -r . --include \*.hs --include \*.md --include \*.txt
