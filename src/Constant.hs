module Constant
  ( progName
  , configName
  , queryMaxLen
  , curName
  , newName
  , mailSep
  ) where

-- Constants

progName :: FilePath
progName = "npop"

configName :: FilePath
configName = "config.toml"

queryKwdLen :: Int
queryKwdLen = 4

queryArgLen :: Int
queryArgLen = 40

queryMaxLen :: Int
queryMaxLen = queryKwdLen + 1 + queryArgLen + 2

curName :: FilePath
curName = "cur"

newName :: FilePath
newName = "new"

mailSep :: FilePath
mailSep = ":2"

