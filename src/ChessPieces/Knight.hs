module ChessPieces.Knight (
  isValidMove
) where

import ChessTypes

isValidMove :: MoveData -> Location -> Bool
isValidMove (_, _, KCastle) (c, r) = False
isValidMove (_, _, QCastle) (c, r) = False
isValidMove (_, Nothing, MoveLocation (x, y)) (c, r)
  | diff == (2,1)     = True
  | diff == (2, -1)   = True
  | diff == (1, 2)    = True
  | diff == (1, -2)   = True
  | diff == (-1, 2)   = True
  | diff == (-1, -2)  = True
  | diff == (-2, 1)   = True
  | diff == (-2, -1)  = True
  | otherwise = False
  where colDiff = (fromEnum x) - (fromEnum c)
        rowDiff = (fromEnum y) - (fromEnum r)
        diff = (colDiff, rowDiff)

isValidMove (_, Just (Left s), MoveLocation (x, y)) (c, r) = c == s
isValidMove (_, Just (Right s), MoveLocation (x, y)) (c, r) = r == s
