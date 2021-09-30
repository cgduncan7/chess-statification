module PGN (
  MetaData,
  Move,
  MoveData,
  GameData(GameData),
  GameData',
  getMetadata,
  getMetadataValue,
  getMovedata,
  extractGamedata,
  extractMetadataKeyValue,
  extractMetadata,
  extractMovedata,
  parseMovedataValue,
  parsePieceName,
  parseStartingLocation,
  parseMoveLocation
) where

import Debug.Trace (trace)

import qualified Data.Text as T
import qualified Data.Map as M
import Text.Regex.TDFA

import ChessPieces.Piece (PieceName(..))
import ChessTypes

type MetaData = M.Map String String

data GameData = GameData { metadata :: MetaData
                         , movedata :: [MoveData]
                         } deriving (Show, Read, Eq)

type GameData' = Either GameData String

getMetadata :: GameData' -> MetaData
getMetadata x = case x of
  Right s                    -> error s
  Left (GameData metadata _) -> metadata

getMetadataValue :: (GameData') -> String -> Maybe String
getMetadataValue l s = M.lookup s (getMetadata l)

getMovedata :: GameData' -> [MoveData]
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
      getGroups (_, _, _, [a, b]) = (a, b)
  in getGroups ((x =~ p) :: (String, String, String, [String]))

extractMetadata :: [String] -> MetaData
extractMetadata [] = M.empty
extractMetadata x =
  let prefix = T.pack "["
      array = map T.pack x
      mapUnpack = map T.unpack
      mapKeyValue = map extractMetadataKeyValue
  in M.fromList (mapKeyValue (mapUnpack (filter (T.isPrefixOf prefix) array)))

extractMovedata :: [String] -> [MoveData]
extractMovedata [] = []
extractMovedata x =
  let prefix = T.pack "1."
      mapPack = map T.pack
      mapUnpack = map T.unpack
      mapParse = map parseMovedataValue
      pattern = "([A-Za-z][A-Za-z0-9]+\\+?#?)|(O-O)|(O-O-O)" -- Pattern to find all moves and remove turn counters
  in mapParse (getAllTextMatches (mapUnpack (filter (T.isPrefixOf prefix) (mapPack x)) !! 0 =~ pattern) :: [String])

parseMovedataValue :: String -> MoveData
parseMovedataValue xs
  | xs == "O-O"   = ([King, Rook], Nothing, KCastle)
  | xs == "O-O-O" = ([King, Rook], Nothing, QCastle)
  | otherwise     = ([parsePieceName xs], parseStartingLocation xs, parseMoveLocation xs)

parsePieceName :: String -> PieceName
parsePieceName (x:xs)
  | x == 'N'  = Knight
  | x == 'B'  = Bishop
  | x == 'R'  = Rook
  | x == 'Q'  = Queen
  | x == 'K'  = King
  | otherwise = Pawn

-- parseStartingLocation takes a PGN move string ("Nf3") and will return `Maybe (Either (Either Rank File) Location)`
-- This function will most of the time return `Nothing` unless there are more details in the PGN move string to remove ambiguity of which piece
-- performed the move. An example of this is when there are Knights on d2 and e3 and a move "Nc4" was recorded. Both Knights can perform this
-- move and as a result additional information is given in PGN such as "Ndc4" or "Nec4" depending on which Knight performed the move.
-- The order of additional information is: File > Rank > Location
parseStartingLocation :: String -> MoveStartingLocation
parseStartingLocation xs =
  let fileRegex = "[BKNQR]([a-h])x?[a-h][1-8][#\\+]?"
      rankRegex = "[BKNQR]([1-8])x?[a-h][1-8][#\\+]?"
      locationRegex = "[BKNQR]([a-h])([1-8])x?[a-h][1-8][#\\+]?"
      notNull x = not (null x)
      filterNotNull = filter notNull
      f (_, _, _, fileGroup) (_, _, _, rankGroup) (_, _, _, locationGroup)
        | length (filterNotNull fileGroup) == 1     = Just (Left (Left (pgnFileToFile $ fileGroup !! 0 !! 0)))
        | length (filterNotNull rankGroup) == 1     = Just (Left (Right (pgnRankToRank $ rankGroup !! 0 !! 0)))
        | length (filterNotNull locationGroup) == 1 = Just (Right ((pgnFileToFile $ locationGroup !! 0 !! 0), (pgnRankToRank $ locationGroup !! 0 !! 1)))
        | otherwise      = Nothing
  in f (xs =~ fileRegex :: (String, String, String, [String])) (xs =~ rankRegex :: (String, String, String, [String])) (xs =~ locationRegex :: (String, String, String, [String]))

parseMoveLocation :: String -> Move
parseMoveLocation xs =
  let p = "[a-h][1-8][#\\+]?"
      g (x:y:_) = (pgnFileToFile x, pgnRankToRank y)
  in MoveLocation $ g (xs =~ p :: String)