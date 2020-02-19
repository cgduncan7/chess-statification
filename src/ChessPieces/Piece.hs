module ChessPieces.Piece (
  PieceRank(..),
  Piece(..),
  isValidMove
) where

import ChessTypes
import qualified ChessPieces.Pawn as Pawn
import qualified ChessPieces.Knight as Knight

data PieceRank = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Read, Eq)

data Piece = Piece PieceRank Location Color deriving (Show, Read, Eq)

isValidMove :: String -> Piece -> Bool
isValidMove s (Piece p l _) = 
  case p of
    Pawn   -> Pawn.isValidMove s l
    Knight -> Knight.isValidMove s l