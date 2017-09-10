#!/bin/bash

grep $1 -r . --include \*.hs --include \*.md --include \*.txt
