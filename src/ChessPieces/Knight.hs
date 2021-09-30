module ChessPieces.Knight (
  isValidMove
) where

import ChessTypes

isValidMove :: MoveData -> Location -> Bool
isValidMove (_, _, KCastle) _ = False
isValidMove (_, _, QCastle) _ = False
isValidMove (_, _, MoveLocation (f, r)) (f', r')
  | diff == (2, 1)    = True
  | diff == (2, -1)   = True
  | diff == (1, 2)    = True
  | diff == (1, -2)   = True
  | diff == (-1, 2)   = True
  | diff == (-1, -2)  = True
  | diff == (-2, 1)   = True
  | diff == (-2, -1)  = True
  | otherwise = False
  where fileDiff = (fromEnum f) - (fromEnum f')
        rankDiff = (fromEnum r) - (fromEnum r')
        diff = (fileDiff, rankDiff)
