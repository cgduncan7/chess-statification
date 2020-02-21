module ChessPieces.Piece (
  PieceRank(..),
  Piece(..),
  getPieceRank,
  getPieceLocation,
  getPieceColor,
  getPiecesByColor,
  getPiecesByRank,
  getPiecesByColorAndRank,
  getValidPieceForMove,
  isValidMove
) where

import ChessTypes
import qualified ChessPieces.Pawn as Pawn
import qualified ChessPieces.Knight as Knight

data Piece = Piece PieceRank Location Color deriving (Show, Read, Eq)

getPieceRank :: Piece -> PieceRank
getPieceRank (Piece r _ _) = r

getPieceLocation :: Piece -> Location
getPieceLocation (Piece _ l _) = l

getPieceColor :: Piece -> Color
getPieceColor (Piece _ _ c) = c

getPiecesByColor :: Color -> [Piece] -> [Piece]
getPiecesByColor c p =
  let match x = (getPieceColor x) == c
  in filter match p

getPiecesByRank :: [PieceRank] -> [Piece] -> [Piece]
getPiecesByRank r p =
  let match x = (getPieceRank x) `elem` r
  in filter match p

getPiecesByColorAndRank :: [Piece] -> Color -> [PieceRank] -> [Piece]
getPiecesByColorAndRank p c r = getPiecesByColor c (getPiecesByRank r p)

getValidPieceForMove :: MoveData -> Color -> [Piece] -> Piece
getValidPieceForMove (rs, s, m) c p =
  let avail = getPiecesByColorAndRank p c rs
      isValid x = isValidMove (rs, s, m) x
  in filter isValid avail !! 0

isValidMove :: MoveData -> Piece -> Bool
isValidMove m (Piece p l _) = 
  case p of
    Pawn   -> Pawn.isValidMove m l
    Knight -> Knight.isValidMove m l