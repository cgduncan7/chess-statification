module ChessPieces.Piece (
  PieceName(..),
  Piece(..),
  getPieceRank,
  getPieceLocation,
  getPieceColor,
  getPiecesByColor,
  getPiecesByRank,
  getPiecesByColorAndRank,
  getValidPiecesForMove,
  isValidMove,
  movePieces,
  movePiece
) where

import Debug.Trace (trace)

import ChessTypes
import qualified ChessPieces.Pawn as Pawn
import qualified ChessPieces.Knight as Knight

data Piece = Piece PieceName Location Color deriving (Show, Read, Eq)

getPieceRank :: Piece -> PieceName
getPieceRank (Piece r _ _) = r

getPieceLocation :: Piece -> Location
getPieceLocation (Piece _ l _) = l

getPieceColor :: Piece -> Color
getPieceColor (Piece _ _ c) = c

getPiecesByColor :: Color -> [Piece] -> [Piece]
getPiecesByColor c p =
  let match x = (getPieceColor x) == c
  in filter match p

getPiecesByRank :: [PieceName] -> [Piece] -> [Piece]
getPiecesByRank r p =
  let match x = (getPieceRank x) `elem` r
  in filter match p

getPiecesByColorAndRank :: [Piece] -> Color -> [PieceName] -> [Piece]
getPiecesByColorAndRank p c r = getPiecesByColor c (getPiecesByRank r p)

getValidPiecesForMove :: MoveData -> Color -> [Piece] -> [Piece]
getValidPiecesForMove (rs, s, m) c p =
  let avail = getPiecesByColorAndRank p c rs
      isValid x = isValidMove (rs, s, m) x
  in filter isValid avail

getInvalidPiecesForMove :: MoveData -> Color -> [Piece] -> [Piece]
getInvalidPiecesForMove (rs, s, m) c p =
  let isValid x = not $ isValidMove (rs, s, m) x
  in filter isValid p

isValidMove :: MoveData -> Piece -> Bool
isValidMove m (Piece p l _) = 
  case p of
    Pawn      -> Pawn.isValidMove m l
    Knight    -> Knight.isValidMove m l
    otherwise -> False
  
movePieces :: MoveData -> Color -> [Piece] -> [Piece]
movePieces md c pieces =
  let movingPieces = getValidPiecesForMove md c pieces
      stationaryPieces = getInvalidPiecesForMove md c pieces
      mapMove pieces = map (movePiece md) pieces
  in concat [mapMove movingPieces, stationaryPieces]

movePiece :: MoveData -> Piece -> Piece
movePiece (_,  _, (MoveLocation (f, r))) (Piece p _ c) =
  (Piece p (f, r) c)