module ChessTypes (
  Row(..),
  pgnRowToRow,
  Column(..),
  pgnColToColumn,
  Location,
  Color(..)
)
where

data Row = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Read, Show, Enum, Eq, Ord)

pgnRowToRow :: Char -> Row
pgnRowToRow r = case r of
  '1' -> One
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight

data Column = A | B | C | D | E | F | G | H 
  deriving (Read, Show, Enum, Eq, Ord)

pgnColToColumn :: Char -> Column
pgnColToColumn c = case c of
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

type Location = (Column, Row)