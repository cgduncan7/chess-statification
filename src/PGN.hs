module PGN (
  MetaData,
  GameData(GameData),
  GameData',
  getMetadata,
  getMetadataValue,
  getMovedata,
  extractGamedata,
  extractMetadataKeyValue,
  extractMetadata,
  extractMovedata
) where

import qualified Data.Text as T
import qualified Data.Map as M
import Text.Regex.TDFA

type MetaData = M.Map String String

data GameData = GameData { metadata :: MetaData
                         , movedata :: [String]
                         } deriving (Show, Eq)

type GameData' = Either GameData String

getMetadata :: GameData' -> MetaData
getMetadata x = case x of
  Right s                    -> error s
  Left (GameData metadata _) -> metadata

getMetadataValue :: (GameData') -> String -> Maybe String
getMetadataValue l s = M.lookup s (getMetadata l)

getMovedata :: GameData' -> [String]
getMovedata x = case x of
  Right s                    -> error s
  Left (GameData _ movedata) -> movedata

extractGamedata :: String -> GameData'
extractGamedata "" = Right "Empty string provided"
extractGamedata x = Left (GameData (extractMetadata lines) (extractMovedata lines))
  where lines = map T.unpack (T.lines (T.pack x))

extractMetadataKeyValue :: String -> (String, String)
extractMetadataKeyValue x = 
  let p = "\\[([A-Za-z]+) \"(.+)\"\\]"
      matches = (x =~ p) :: (String, String, String, [String])
      getGroups (_, _, _, [a, b]) = (a, b)
  in getGroups matches

extractMetadata :: [String] -> MetaData
extractMetadata [] = M.empty
extractMetadata x =
  let prefix = T.pack "["
      array = map T.pack x
      mapUnpack = map T.unpack
      mapKeyValue = map extractMetadataKeyValue
  in M.fromList (mapKeyValue (mapUnpack (filter (T.isPrefixOf prefix) array)))

extractMovedata :: [String] -> [String]
extractMovedata [] = []
extractMovedata x =
  let prefix = T.pack "1."
      array = map T.pack x
      mapUnpack = map T.unpack
      line = mapUnpack (filter (T.isPrefixOf prefix) array) !! 0
      pattern = "([A-Za-z][A-Za-z0-9]+\\+?#?)|(O-O)|(O-O-O)" -- Pattern to find all moves and remove turn counters
  in getAllTextMatches (line =~ pattern) :: [String]