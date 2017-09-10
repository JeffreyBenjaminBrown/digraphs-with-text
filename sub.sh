#!/bin/bash

# The first generation of Either-returning functions returned Either String.
# The new one return Either DwtErr.
  # They had the same name plus "De" at the end.
# This changes names: Give it an argument X, and it changes (order important!)
  # X to XStrErr
  # XDe to X

find . -name "*.hs" -print0 -o -name "*.txt" -print0 -o -name "*.md" -print0 | xargs -0 sed -i -r "s/([^a-zA-Z0-9\d]|^)$1([^a-zA-Z0-9\d]|$)/\1$1StrErr\2/g"

find . -name "*.hs" -print0 -o -name "*.txt" -print0 -o -name "*.md" -print0 | xargs -0 sed -i -r "s/([^a-zA-Z0-9\d]|^)$1De([^a-zA-Z0-9\d]|$)/\1$1\2/g"

echo "Here's a handy commit message:"
echo "change names: $1De -> $1 -> $1StrErr"
echo "If the function name is a common English word, or if it might have been used in comments, don't forget to check the diff!"
