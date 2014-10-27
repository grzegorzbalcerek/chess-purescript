module Chess.Game where

import Data.Maybe
import qualified Data.Map as M
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Chess.FigureMoves
import Chess.Board
import Chess.Util
import Chess.Color
import Data.Foldable (mconcat,foldr,any,all,elem,find)
import Data.Tuple
import Math
import qualified Data.Array as A

data Game = GameStart
          | OngoingGame Color Board Game Move

gameColor game = case game of
  GameStart -> White
  OngoingGame color _ _ _ -> color

gameHist :: Game -> [Game]
gameHist game = case game of
  GameStart -> [GameStart]
  OngoingGame _ _ prevGame _ -> game : gameHist prevGame

gameBoard game = case game of
  GameStart -> gameStartBoard
  OngoingGame _ board _ _ -> board

{-
-}
gameMoves :: Game -> [String]
gameMoves game = gameHist game >>= \g ->
  case g of
    GameStart -> []
    OngoingGame _ board _ move -> [showMove move board]

{-
:i Chess.Game
showGameHist GameStart
-}
showGameHist :: Game -> String
showGameHist game =
  let moves = gameMoves game
      separator num = if num % 2 == 0 then " " else "\n"
      moveNumber num = if num % 2 == 0 then mconcat ["   ", (show (1 + floor (num / 2))),". "] else ""
      step move (Tuple num str) = Tuple (num+1) (mconcat [str, moveNumber num, move, separator num])
  in
     snd $ foldr step (Tuple 0 "") moves

{-
:i Chess.Game
gameMessage GameStart
-}
gameMessage :: Game -> String
gameMessage game = case game of
  GameStart -> "White to begin"
  OngoingGame color board _ lastMove ->
    "Last move: " ++ (show $ other color) ++ " " ++
    (showField $ Chess.Move.from lastMove) ++ " to " ++
    (showField $ Chess.Move.to lastMove)

{-
:i Chess.Game
showGame GameStart
-}
showGame :: Game -> String
showGame game = mconcat [gameMessage game, showBoard (gameBoard game)]

instance showGameInstance :: Show Game where
  show = showGame

{-
:i Chess.Game
:i Chess.Field
isFieldEmpty GameStart (Field 2 2)
false
isFieldEmpty GameStart (Field 2 3)
true
-}
isFieldEmpty game field = not (M.member field (gameBoard game))

{-
:i Chess.Game
:i Chess.FigureMoves
:i Chess.Field
:i Chess.Figure
:i Chess.Color
:i Data.Array
map showField $ freeDestinations GameStart $ figureMoves (Figure Rook White) (Field 3 4) false
["d4","e4","f4","g4","h4","b4","a4","c5","c6","c3"]
map showField $ freeDestinations GameStart $ figureMoves (Figure Bishop White) (Field 3 4) false
["d5","e6","b5","a6","d3","b3"]
-}
freeDestinations :: Game -> [[Field]] -> [Field]
freeDestinations game fieldss =
  A.concatMap (\fields -> takeWhile (isFieldEmpty game) fields) fieldss

{-
:i Chess.Game
:i Chess.FigureMoves
:i Chess.Field
:i Chess.Figure
:i Chess.Color
:i Data.Array
map showField $ captureDestinations GameStart $ figureMoves (Figure Rook White) (Field 3 4) false
["c7"]
map showField $ captureDestinations GameStart $ figureMoves (Figure Bishop White) (Field 3 4) false
["f7"]
-}
captureDestinations :: Game -> [[Field]] -> [Field]
captureDestinations game =
  let hasEnemyFigure field =
        figureColor <$> (M.lookup field (gameBoard game)) == Just $ other (gameColor game)
  in
    A.concatMap $ A.filter hasEnemyFigure <<<
                  A.take 1 <<<
                  dropWhile (isFieldEmpty game)

{-
-}
defendedDestinations :: Game -> [[Field]] -> [Field]
defendedDestinations game =
  let hasSameColorFigure field =
        figureColor <$> (M.lookup field (gameBoard game)) == Just (gameColor game)
  in
    A.concatMap $ A.filter hasSameColorFigure <<<
                  A.take 1 <<<
                  dropWhile (isFieldEmpty game)

{-
-}
updateGame :: Game -> Move -> Game
updateGame game move =
  OngoingGame (other (gameColor game))
              (updateBoard (gameBoard game) move)
              game
              move

{-
    > isOtherKingUnderCheck GameStart
--    False : Bool
-}
isOtherKingUnderCheck :: Game -> Boolean
isOtherKingUnderCheck game =
  let isKingOnBoard g = any (\x -> x == Figure King (other $ gameColor game))
                            (M.values (gameBoard g))
  in
    not $ all isKingOnBoard (nextGames game)

--{-|
--
--Verifies if the King of the player who is about to make a move is under check.
--
--    > isKingUnderCheck GameStart
--    False : Bool
--
---}
--
isKingUnderCheck :: Game -> Boolean
isKingUnderCheck game =
  let newGame = OngoingGame (other $ gameColor game)
                            (gameBoard game)
                            game
                            (RegularMove (Field 0 0) (Field 0 0))
  in isOtherKingUnderCheck newGame

{-
-}
castling :: Game -> Number -> Number -> Number -> Number -> [Game]
castling game kingTo rookFrom rookTo otherCol =
  let color = gameColor game
      row = firstRow color
      hist = gameHist game
      board = gameBoard game
  in if M.lookup (Field 5 row) board == Just (Figure King color) &&
        M.lookup (Field rookFrom row) board == Just (Figure Rook color) &&
        M.lookup (Field rookTo row) board == Nothing &&
        M.lookup (Field kingTo row) board == Nothing &&
        M.lookup (Field otherCol row) board == Nothing &&
        all (\g -> M.lookup (Field 5 row) (gameBoard g) == Just (Figure King color)) hist &&
        all (\g -> M.lookup (Field rookFrom row) (gameBoard g) == Just (Figure Rook color)) hist &&
        not (isOtherKingUnderCheck (updateGame game (RegularMove (Field 5 row) (Field rookTo row))))
     then [updateGame game (CastlingMove (Field 5 row) (Field kingTo row) (Field rookFrom row) (Field rookTo row))]
     else []

{-
-}
isEnPassantCapture :: Game -> Field -> Field -> Boolean
isEnPassantCapture game from to = case game of
  GameStart -> false
  OngoingGame color board _ lastMove ->
    M.lookup (Chess.Move.to lastMove) board == Just (Figure Pawn (other color)) &&
    Chess.Move.to lastMove == Field (col to) (row from) &&
    Chess.Move.from lastMove == Field (col to) (row from + 2 * (row to - row from))

{-
:i Chess.Game
nextGames GameStart
-}
nextGames :: Game -> [Game]
nextGames game =
    A.filter (\(Tuple _ fig) -> figureColor fig == gameColor game) (M.toList $ gameBoard game) >>= \(Tuple from fig) ->
    let figType = figureType fig
    in
      if figType /= Pawn
      then
        (let fieldss = figureMoves fig from true
         in A.map (\to -> updateGame game (RegularMove from to)) $
            freeDestinations game fieldss ++ captureDestinations game fieldss) ++
        (if figType == King
         then castling game 3 1 4 2 ++ castling game 7 8 6 7
         else [])
      else
        let regularAndPromotionMoves =
              mconcat [ freeDestinations game (figureMoves fig from false)
                      , captureDestinations game (figureMoves fig from true) ] >>=
                \to -> if isLastRow to (gameColor game)
                       then A.map (\figType -> updateGame game $ PromotionMove from to $
                                               Figure figType $ gameColor game)
                            [ Queen, Rook, Bishop, Knight ]
                       else [ updateGame game (RegularMove from to) ]
            enPassantMoves =
              A.map (\to -> updateGame game (EnPassantMove from to (Field (col to) (row from)))) <<<
              A.filter (isEnPassantCapture game from) $
              freeDestinations game (figureMoves fig from true)
        in regularAndPromotionMoves ++ enPassantMoves

{-
-}
validGames :: Game -> [Game]
validGames game = A.filter (not <<< isOtherKingUnderCheck) $ nextGames game

{-
-}
validPromotionsMoves :: Game -> Field -> Field -> [Figure]
validPromotionsMoves game from to =
  if isLastRow to (gameColor game) &&
     (figureType <$> (M.lookup from (gameBoard game))) == Just Pawn
  then validGames game >>= \nextGame ->
       case nextGame of
         OngoingGame _ _ _ (PromotionMove f t fig) -> if f == from && t == to then [fig] else []
         _ -> []
  else []

{-|
--
--Verifies whether the game is over.  The following end game conditions
--are handled:
--
--  * after every possible move the King is under check,
--  * only the two Kings are left on the board,
--  * only the two Kings, one Bishop and one Knight are left on the board,
--  * only the two Kings and two Knights of the same color are left on the board,
--  * the same position occurred three times.
--
--    > isGameFinished GameStart
--    False : Bool
--
-}
isGameFinished :: Game -> Boolean
isGameFinished game =
  all isOtherKingUnderCheck (nextGames game) ||
  elem (A.map showFigure $ M.values (gameBoard game))
       [ A.sort [ "k", "K" ],
         A.sort [ "k", "K" , "b" ],
         A.sort [ "k", "K" , "B" ],
         A.sort [ "k", "K" , "n" ],
         A.sort [ "k", "K" , "N" ],
         A.sort [ "k", "K" , "n" , "n" ],
         A.sort [ "k", "K" , "N" , "N" ]] ||
  (not <<< A.null <<< A.filter (\g -> A.length g >= 3) <<< A.group <<< A.sort <<< A.map (showBoard <<< gameBoard) $ gameHist game)

{-
-}
winner :: Game -> Maybe Color
winner game =
  if (isGameFinished game && isKingUnderCheck game)
  then Just <<< other <<< gameColor $ game
  else Nothing

{-
-}
makeMove :: Field -> Field -> Maybe Figure -> Game -> Maybe Game
makeMove from to promotion game =
  let isMatching (OngoingGame _ _ _ move) =
          case move of
            RegularMove f t -> f == from && t == to && isNothing promotion
            PromotionMove f t fig -> f == from && t == to && promotion == Just fig
            EnPassantMove f t _ -> f == from && t == to && isNothing promotion
            CastlingMove f t _ _ ->  f == from && t == to && isNothing promotion
  in
    find (const true) $ A.filter isMatching $ validGames game
