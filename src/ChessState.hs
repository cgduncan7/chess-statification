module ChessState (
  GameStatus(..),
  GameState(GameState),
  getGSPieces,
  getGSStatus,
  getGSTurn,
  Player(Player),
  Result(..),
  Game(Game),
  getGameStates,
  Game',
  gamedataToGame
) where

import Debug.Trace (trace)

import Data.Maybe (fromJust)

import PGN (MoveData, GameData, GameData', getMetadataValue, getMovedata)
import ChessTypes
import ChessPieces.Piece

data GameStatus = Check | Checkmate | Stalemate | Normal deriving (Show, Read, Eq)

data GameState = GameState { gsPieces :: [Piece]
                           , gsStatus :: GameStatus
                           , gsTurn :: Color
                           } deriving (Show, Read, Eq)

getGSPieces :: GameState -> [Piece]
getGSPieces (GameState pieces _ _) = pieces

getGSStatus :: GameState -> GameStatus
getGSStatus (GameState _ status _) = status

getGSTurn :: GameState -> Color
getGSTurn (GameState _ _ turn) = turn

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

getGameStates :: Game -> [GameState]
getGameStates (Game _ s _ _) = s

type Game' = Maybe Game

gamedataToGame :: GameData' -> Game
gamedataToGame x = case x of
  Right s -> error s
  Left _  ->
    let id = "1"
        states = movedataToGameStates $ getMovedata x
        wn = fromJust $ getMetadataValue x "White"
        we = fromJust $ getMetadataValue x "WhiteElo"
        bn = fromJust $ getMetadataValue x "Black"
        be = fromJust $ getMetadataValue x "BlackElo"
        w = Player wn we White
        b = Player bn be Black
        players = (w, b)
        result = pgnToResult $ fromJust $ getMetadataValue x "Result"
    in Game id states players result

defaultPieces = [ Piece Pawn (A, Two) White
                , Piece Pawn (B, Two) White
                , Piece Pawn (C, Two) White
                , Piece Pawn (D, Two) White
                , Piece Pawn (E, Two) White
                , Piece Pawn (F, Two) White
                , Piece Pawn (G, Two) White
                , Piece Pawn (H, Two) White
                , Piece Rook (A, One) White
                , Piece Knight (B, One) White
                , Piece Bishop (C, One) White
                , Piece Queen (D, One) White
                , Piece King (E, One) White
                , Piece Bishop (F, One) White
                , Piece Knight (G, One) White
                , Piece Rook (H, One) White
                , Piece Pawn (A, Seven) Black
                , Piece Pawn (B, Seven) Black
                , Piece Pawn (C, Seven) Black
                , Piece Pawn (D, Seven) Black
                , Piece Pawn (E, Seven) Black
                , Piece Pawn (F, Seven) Black
                , Piece Pawn (G, Seven) Black
                , Piece Pawn (H, Seven) Black
                , Piece Rook (A, Eight) Black
                , Piece Knight (B, Eight) Black
                , Piece Bishop (C, Eight) Black
                , Piece Queen (D, Eight) Black
                , Piece King (E, Eight) Black
                , Piece Bishop (F, Eight) Black
                , Piece Knight (G, Eight) Black
                , Piece Rook (H, Eight) Black
                ]

defaultState = GameState defaultPieces Normal White

movedataToGameStates :: [MoveData] -> [GameState]
movedataToGameStates xs = foldl (\acc x -> acc ++ [movedataToGameState (last acc) x]) [defaultState] xs

movedataToGameState :: GameState -> MoveData -> GameState
movedataToGameState s m =
  let turn = getGSTurn s
      pieces = getGSPieces s
      movedPieces = movePieces m turn pieces
      nextTurn = toEnum $ (fromEnum turn + 1) `mod` 2 :: Color
  in trace ("movedataToGameState" ++ " -- " ++ show m ++ "\n" ++ show movedPieces) $ (GameState movedPieces Normal nextTurn)
  --in (GameState movedPieces Normal nextTurn)