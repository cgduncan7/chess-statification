module ChessPieces.Pawn (
  isValidMove
) where

import ChessTypes

isValidMove :: MoveData -> Location -> Bool
isValidMove m l = True