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
  parsePieceRank,
  parseStartingLocation,
  parseMoveLocation
) where

import qualified Data.Text as T
import qualified Data.Map as M
import Text.Regex.TDFA

import ChessPieces.Piece (PieceRank(..))
import ChessTypes

type MetaData = M.Map String String

data Move = MoveLocation Location | KCastle | QCastle deriving (Show, Read, Eq)

type MoveData = ([PieceRank],  Maybe (Either Column Row), Move)

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

parseMovedataValue :: String -> ([PieceRank], Maybe (Either Column Row), Move)
parseMovedataValue xs
  | xs == "O-O"   = ([King, Rook], Nothing, KCastle)
  | xs == "O-O-O" = ([King, Rook], Nothing, QCastle)
  | otherwise     = ([parsePieceRank xs], parseStartingLocation xs, parseMoveLocation xs)

parsePieceRank :: String -> PieceRank
parsePieceRank (x:xs)
  | x == 'N'  = Knight
  | x == 'B'  = Bishop
  | x == 'R'  = Rook
  | x == 'Q'  = Queen
  | x == 'K'  = King
  | otherwise = Pawn

parseStartingLocation :: String -> Maybe (Either Column Row)
parseStartingLocation xs =
  let c = "[BKNQR]([a-h]?)x?[a-f][1-8][#\\+]?"
      r = "[BKNQR]([1-8]?)x?[a-f][1-8][#\\+]?"
      n x = not (null x)
      b = filter n
      f (_, _, _, cm) (_, _, _, rm)
        | length (b cm) == 1 = Just (Left (pgnColToColumn $ cm !! 0 !! 0))
        | length (b rm) == 1 = Just (Right (pgnRowToRow $ rm !! 0 !! 0))
        | otherwise      = Nothing
  in f (xs =~ c :: (String, String, String, [String])) (xs =~ r :: (String, String, String, [String]))

parseMoveLocation :: String -> Move
parseMoveLocation xs =
  let p = "[a-h][1-8][#\\+]?"
      g (x:y:_) = (pgnColToColumn x, pgnRowToRow y)
  in MoveLocation $ g (xs =~ p :: String)