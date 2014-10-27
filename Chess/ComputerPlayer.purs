module Chess.ComputerPlayer where

import Data.Array
import Data.Tuple
import Data.Maybe
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Chess.FigureMoves
import Chess.Game
import Chess.Rank
import Chess.Util

{-| Returns a sequence of the best ranked moves.-}
moves :: Game -> [Game]
moves game =
  let mvs = validGames game
  in if null mvs
     then []
     else let
            rankedMoves :: [Tuple Game Number]
            rankedMoves = map (\g -> Tuple g (rank g (gameColor g))) mvs
            rankedMovesSorted :: [Tuple Game Number]
            rankedMovesSorted = sortBy (\x y -> compare (snd x) (snd y)) rankedMoves
            firstRank :: Maybe Number
            firstRank = snd <$> head rankedMovesSorted
            maxRankMoves = takeWhile (\(Tuple _ rank) -> Just rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

{-
PS.Chess_Game.showGame(PS.Chess_ComputerPlayer.generateMove(PS.Chess_Game.GameStart.value).value0)
var g1 = PS.Chess_ComputerPlayer.generateMove(PS.Chess_Game.GameStart.value)
var g2 = PS.Chess_ComputerPlayer.generateMove(g1.value0)
PS.Chess_Game.showGame(g2.value0)
-}
generateMove :: Game -> Maybe Game
generateMove game = case moves game of
  [] -> Nothing
  h:_ -> Just h

{-
var generateMove = PS.Chess_ComputerPlayer.generateMove
var GameStart = PS.Chess_Game.GameStart.value
var showGame = PS.Chess_Game.showGame
var RegularMove = PS.Chess_Move.RegularMove
var Field = PS.Chess_Field.Field
var updateGame = PS.Chess_Game.updateGame
var g1 = updateGame(GameStart)(RegularMove.create(new Field(2,2))(new Field(2,3)))
showGame(g1)
var g2 = generateMove(g1)
showGame(g2.value0)
-}
