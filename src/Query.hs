{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Query
  ( Query(..)
  , QueryErr(..)
  , buildQuery
  ) where

import Data.Char (toUpper)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Server (ClientErr(..))

data Query
  = User ByteString
  | Pass ByteString
  | Stat
  | List (Maybe Word)
  | Uidl (Maybe Word) 
  | Retr Word
  | Dele Word
  | Noop
  | Rset
  | Quit
  deriving Show

data QueryErr
  = Client ClientErr
  | Empty
  | Unknown
  | Malformed

instance Show QueryErr where
  show = \case
    Client err -> show err
    Empty      -> "empty query"
    Unknown    -> "unknown command"
    Malformed  -> "malformed command"

maxWord :: Integer
maxWord = toInteger (maxBound :: Word)

readWord :: BS.ByteString -> Maybe Word
readWord bs = do
    (n, rest) <- BS.readInteger bs

    if BS.null rest
       && n >= 0
       && n <= maxWord
    then Just (fromInteger n)
    else Nothing

parseVoid :: Query -> [ByteString] -> Either QueryErr Query
parseVoid q [] = Right q
parseVoid _ _  = Left Malformed

parseWord :: (Word -> Query) -> [ByteString] -> Either QueryErr Query
parseWord q [arg] = maybe (Left Malformed) (Right . q) (readWord arg)
parseWord _  _    = Left Malformed

parseMaybeWord :: (Maybe Word -> Query) -> [ByteString] -> Either QueryErr Query
parseMaybeWord q []    = Right (q Nothing)
parseMaybeWord q [arg] = parseWord (q . Just) [arg]
parseMaybeWord _  _    = Left Malformed

parseString :: (ByteString -> Query) -> [ByteString] -> Either QueryErr Query
parseString q [arg] = Right (q arg)
parseString _ _     = Left Malformed

upperFirst :: [ByteString] -> Maybe (ByteString, [ByteString])
upperFirst []     = Nothing
upperFirst (x:xs) = Just (BS.map toUpper x, xs)

tokenize :: ByteString -> Maybe (ByteString, [ByteString])
tokenize = upperFirst . BS.words

parsers :: [(ByteString, [ByteString] -> Either QueryErr Query)]
parsers = 
  [ ("USER", parseString User)
  , ("PASS", parseString Pass)
  , ("LIST", parseMaybeWord List)
  , ("UIDL", parseMaybeWord Uidl)
  , ("RETR", parseWord Retr)
  , ("DELE", parseWord Dele)
  , ("STAT", parseVoid Stat)
  , ("NOOP", parseVoid Noop)
  , ("RSET", parseVoid Rset)
  , ("QUIT", parseVoid Quit)
  ]

parserOf :: ByteString -> Maybe ([ByteString] -> Either QueryErr Query)
parserOf = flip lookup parsers

parseQuery :: ByteString -> Either QueryErr Query
parseQuery line = case tokenize line of
  Nothing          -> Left Empty
  Just (cmd, args) ->
    case parserOf cmd of
      Just parser -> parser args
      Nothing     -> Left Unknown
    
buildQuery :: Either ClientErr ByteString -> Either QueryErr Query
buildQuery = either (Left . Client) parseQuery
