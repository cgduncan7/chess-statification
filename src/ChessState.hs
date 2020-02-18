module ChessState (
  Row(..),
  Column(..),
  Captured,
  Location,
  Color(..),
  PieceRank(..),
  pieceRankToPGN,
  pgnToPieceRank,
  Piece(Piece),
  GameStatus(..),
  GameState(GameState),
  Player(Player),
  Result(..),
  Game(Game),
  Game',
  gamedataToGame
) where

import Data.Maybe (fromJust)
import PGN (GameData, GameData', getMetadataValue)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Read, Show, Enum, Eq, Ord)

data Column = A | B | C | D | E | F | G | H
  deriving (Read, Show, Enum, Eq, Ord)

type Captured = Bool

type Location = (Column, Row, Captured)

data Color = White | Black
  deriving (Read, Show, Enum, Eq, Ord)

data PieceRank = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Read, Show, Enum, Eq, Ord)

pieceRankToPGN :: PieceRank -> Maybe Char
pieceRankToPGN p = case p of
  Pawn -> Nothing
  Knight -> Just 'N'
  Bishop -> Just 'B'
  Rook -> Just 'R'
  Queen -> Just 'Q'
  King -> Just 'K'

pgnToPieceRank :: Maybe Char -> PieceRank
pgnToPieceRank p = case p of
  Nothing -> Pawn
  Just 'N' -> Knight
  Just 'B' -> Bishop
  Just 'R' -> Rook
  Just 'Q' -> Queen
  Just 'K' -> King

data Piece = Piece { pieceId :: String
                   , pieceRank :: PieceRank
                   , pieceColor :: Color
                   , pieceLocation :: Location
                   } deriving (Show, Read, Eq)

data GameStatus = Check | Checkmate | Stalemate | Normal deriving (Show, Read, Eq)

data GameState = GameState { gsid :: String
                           , gsPieces :: [Piece]
                           , gsStatus :: GameStatus
                           , gsTurn :: Color
                           } deriving (Show, Read, Eq)

data Player = Player { playerName :: String
                     , playerELO :: String
                     , playerColor :: Color
                     } deriving (Show, Read, Eq)

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
        states = []
        wn = fromJust $ getMetadataValue x "White"
        we = fromJust $ getMetadataValue x "WhiteElo"
        bn = fromJust $ getMetadataValue x "Black"
        be = fromJust $ getMetadataValue x "BlackElo"
        w = Player wn we White
        b = Player bn be Black
        players = (w, b)
        result = pgnToResult $ fromJust $ getMetadataValue x "Result"
    in Game id states players result