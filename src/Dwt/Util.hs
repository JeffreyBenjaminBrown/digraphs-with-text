    {-# LANGUAGE FlexibleContexts #-}
    module Dwt.Util
      ( module Dwt.Util
      ) where

    import qualified Data.Map as Map
    import Control.Monad.Except (MonadError, throwError)

    -- TODO: This could use a higher-kinded function, lke eitherToMe, for Maybes
      -- should in that case take also a String to show if Nothing happens
    mapLookupMe :: (Ord k, Show k, Show a, MonadError String me) -- TODO ? BAD
      => k -> Map.Map k a -> me a -- Is it bad to need this function?
    mapLookupMe k m = case Map.lookup k m of
      Just a -> return a
      Nothing -> throwError $ "mapLookupMe: " ++
      -- reports map; could be bad if map big
        show k ++ " not in map " ++ show m

    eitherToMe :: (Show e, MonadError String me)
      => (a -> Either e t) -> a -> me t
    eitherToMe f x = case f x of Right y -> return y
                                 Left e -> throwError $ show e
