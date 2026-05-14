module Shadow
  ( Shadow
  , loadShadow
  , userExists
  , auth
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Crypto.BCrypt (validatePassword)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Types (Username (unUser))

type Shadow = Map String String
  
loadShadow :: FilePath -> IO Shadow
loadShadow path =
  do
    contents <- readFile path
    pure $ Map.fromList (parseLine <$> lines contents)
  where
    parseLine line =
      let (user, rest) = break (== ':') line
      in  (user, drop 1 rest)

userExists :: Shadow -> String -> Bool
userExists shdw user = Map.member user shdw

auth :: Shadow -> Username -> ByteString -> Bool
auth shadow user pass =
  case Map.lookup (unUser user) shadow of
    Nothing   -> False
    Just hash -> validatePassword (BS.pack hash) pass
