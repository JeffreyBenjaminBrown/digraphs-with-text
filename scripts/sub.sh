#!/bin/bash

# This does half of what sub-2.sh does; see that one for more comments.

IntermediateName=Q # the new way has this suffix removed

find . -name "*.hs" -print0 -o -name "*.cabal" -print0 -o -name "*.txt" -print0 -o -name "*.md" -print0 | xargs -0 sed -i -r "s/([^_a-zA-Z0-9\d]|^)$1$IntermediateName([^_a-zA-Z0-9\d]|$)/\1$1\2/g"

echo "Don't forget to check the diff! The function name might be used in comments."
