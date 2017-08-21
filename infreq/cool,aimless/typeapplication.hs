{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
module Main where

class TypeName t where
    typeName :: String

instance TypeName Bool where
    typeName = "Bool"

instance TypeName () where
    typeName = "()"

main :: IO ()
main = do
    putStrLn $ typeName @Bool
    putStrLn $ typeName @()
