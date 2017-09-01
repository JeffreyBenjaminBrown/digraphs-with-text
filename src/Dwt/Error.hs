module Dwt.Error where

data BaseError = FoundNo | FoundMany | NonTplt | ArityMismatch | Impossible
type DwtError = (BaseError, String)
