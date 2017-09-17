#!/bin/bash

wordChar='[^a-zA-Z0-9_]'
before='(^|[^$wordChar])'
after='($|[^$wordChar])'

egrep "$before$1$after" --exclude-dir=.stack-work \\\
     -r . --include \*.hs --include \*.md --include \*.txt
