module ChessTypes (
  Rank(..),
  pgnRankToRank,
  File(..),
  pgnFileToFile,
  Location,
  Color(..),
  PieceName(..),
  Move(..),
  MoveStartingLocation,
  MoveData(..),
)
where

import Debug.Trace (trace)

-- Rows of the board
data Rank = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Read, Show, Enum, Eq, Ord)

pgnRankToRank :: Char -> Rank
pgnRankToRank r = case r of
  '1' -> One
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight

-- Columns of the board
data File = A | B | C | D | E | F | G | H 
  deriving (Read, Show, Enum, Eq, Ord)

pgnFileToFile :: Char -> File
pgnFileToFile c = case c of
  'a' -> A
  'b' -> B
  'c' -> C
  'd' -> D
  'e' -> E
  'f' -> F
  'g' -> G
  'h' -> H

data Color = White | Black
  deriving (Read, Show, Enum, Eq, Ord)

type Location = (File, Rank)

data PieceName = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Read, Eq)

data Move = MoveLocation Location | KCastle | QCastle deriving (Show, Read, Eq)

type MoveStartingLocation = Maybe (Either (Either File Rank) Location)

type MoveData = ([PieceName],  MoveStartingLocation, Move)
