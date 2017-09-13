#!/bin/bash

# The first generation of Either-returning functions returned Either String.
# The new one return Either DwtErr.
  # They had the same name plus "De" at the end.
# This changes names: Give it an argument X, and it changes (order important!)
  # X to XDeprecatoryName
  # XIntermediateName to X
# To adjust the values of DeprecatoryName and IntermediateName above,
  # just change the strings that immediately follow $1 below

DeprecatoryName=LongErr # the old way is renamed with this suffix
IntermediateName=Sum # the new way has this suffix removed

find . -name "*.hs" -print0 -o -name "*.cabal" -print0 -name "*.txt" -print0 -o -name "*.md" -print0 | xargs -0 sed -i -r "s/([^_a-zA-Z0-9\d]|^)$1([^_a-zA-Z0-9\d]|$)/\1$1$DeprecatoryName\2/g"

find . -name "*.hs" -print0 -o -name "*.cabal" -print0 -name "*.txt" -print0 -o -name "*.md" -print0 | xargs -0 sed -i -r "s/([^_a-zA-Z0-9\d]|^)$1$IntermediateName([^_a-zA-Z0-9\d]|$)/\1$1\2/g"

echo "Here's a handy commit message:"
echo "change names: $1IntermediateName -> $1 -> $1DeprecatoryName"
echo "If the function name is a common English word, or if it might have been used in comments, don't forget to check the diff!"
