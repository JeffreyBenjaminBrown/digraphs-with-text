#!/bin/bash

grep "$1" --exclude-dir=.stack-work \\\
     -r . --include \*.hs

