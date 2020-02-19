module ChessState (
  Row(..),
  Column(..),
  Color(..),
  -- PieceRank(..),
  -- pieceRankToPGN,
  -- pgnToPieceRank,
  -- Piece(Piece),
  GameStatus(..),
  GameState(GameState),
  Player(Player),
  Result(..),
  Game(Game),
  Game',
  gamedataToGame
) where

import Data.Maybe (fromJust)

import PGN (MoveData, GameData, GameData', getMetadataValue, getMovedata)
import ChessTypes
import ChessPieces.Piece

data GameStatus = Check | Checkmate | Stalemate | Normal deriving (Show, Read, Eq)

data GameState = GameState { gsPieces :: [Piece]
                           , gsStatus :: GameStatus
                           , gsTurn :: Color
                           } deriving (Show, Read, Eq)

data Player = Player { playerName :: String
                     , playerELO :: String
                     , playerColor :: Color
                     } deriving (Show, Read, Eq)

instance Ord Player where
  (Player x a _) `compare` (Player y b _) =
    let ab = a `compare` b
        xy = x `compare` y
    in if ab == EQ then xy else ab

data Result = WhiteWins | BlackWins | Draw deriving (Show, Read, Eq)

pgnToResult :: String -> Maybe Result
pgnToResult s
  | s == "1-0"     = Just WhiteWins
  | s == "0-1"     = Just BlackWins
  | s == "1/2-1/2" = Just Draw
  | otherwise      = Nothing

data Game = Game { gid :: String
                 , gStates :: [GameState]
                 , gPlayers :: (Player, Player)
                 , gResult :: Maybe Result
                 } deriving (Show, Read, Eq)

type Game' = Maybe Game

gamedataToGame :: GameData' -> Game
gamedataToGame x = case x of
  Right s -> error s
  Left _  ->
    let id = "1"
        states = movedataToStates $ getMovedata x
        wn = fromJust $ getMetadataValue x "White"
        we = fromJust $ getMetadataValue x "WhiteElo"
        bn = fromJust $ getMetadataValue x "Black"
        be = fromJust $ getMetadataValue x "BlackElo"
        w = Player wn we White
        b = Player bn be Black
        players = (w, b)
        result = pgnToResult $ fromJust $ getMetadataValue x "Result"
    in Game id states players result

-- defaultPieces = [ Piece Pawn White (A, Two, False)
--                 , Piece Pawn White (B, Two, False)
--                 , Piece Pawn White (C, Two, False)
--                 , Piece Pawn White (D, Two, False)
--                 , Piece Pawn White (E, Two, False)
--                 , Piece Pawn White (F, Two, False)
--                 , Piece Pawn White (G, Two, False)
--                 , Piece Pawn White (H, Two, False)
--                 , Piece Rook White (A, One, False)
--                 , Piece Knight White (B, One, False)
--                 , Piece Bishop White (C, One, False)
--                 , Piece Queen White (D, One, False)
--                 , Piece King White (E, One, False)
--                 , Piece Bishop White (F, One, False)
--                 , Piece Knight White (G, One, False)
--                 , Piece Rook White (H, One, False)
--                 , Piece Pawn Black (A, Seven, False)
--                 , Piece Pawn Black (B, Seven, False)
--                 , Piece Pawn Black (C, Seven, False)
--                 , Piece Pawn Black (D, Seven, False)
--                 , Piece Pawn Black (E, Seven, False)
--                 , Piece Pawn Black (F, Seven, False)
--                 , Piece Pawn Black (G, Seven, False)
--                 , Piece Pawn Black (H, Seven, False)
--                 , Piece Rook Black (A, Eight, False)
--                 , Piece Knight Black (B, Eight, False)
--                 , Piece Bishop Black (C, Eight, False)
--                 , Piece Queen Black (D, Eight, False)
--                 , Piece King Black (E, Eight, False)
--                 , Piece Bishop Black (F, Eight, False)
--                 , Piece Knight Black (G, Eight, False)
--                 , Piece Rook Black (H, Eight, False)
--                 ]

movedataToStates :: [MoveData] -> [GameState]
movedataToStates xs = [GameState [] Normal White]
