module Chess.Board where

import qualified Data.Map as M
import Data.Tuple
import Data.String (toUpper)
import Data.Array (map,(..))
import Data.Foldable (mconcat)
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Data.Maybe

type Board = M.Map Field Figure

{-
JSON.stringify(PS.Chess_Board.gameStartBoard)
-}
gameStartBoard :: Board
gameStartBoard = M.fromList [Tuple (Field 1 1) (Figure Rook White)
                            ,Tuple (Field 2 1) (Figure Knight White)
                            ,Tuple (Field 3 1) (Figure Bishop White)
                            ,Tuple (Field 4 1) (Figure Queen White)
                            ,Tuple (Field 5 1) (Figure King White)
                            ,Tuple (Field 6 1) (Figure Bishop White)
                            ,Tuple (Field 7 1) (Figure Knight White)
                            ,Tuple (Field 8 1) (Figure Rook White)
                            ,Tuple (Field 1 2) (Figure Pawn White)
                            ,Tuple (Field 2 2) (Figure Pawn White)
                            ,Tuple (Field 3 2) (Figure Pawn White)
                            ,Tuple (Field 4 2) (Figure Pawn White)
                            ,Tuple (Field 5 2) (Figure Pawn White)
                            ,Tuple (Field 6 2) (Figure Pawn White)
                            ,Tuple (Field 7 2) (Figure Pawn White)
                            ,Tuple (Field 8 2) (Figure Pawn White)
                            ,Tuple (Field 1 7) (Figure Pawn Black)
                            ,Tuple (Field 2 7) (Figure Pawn Black)
                            ,Tuple (Field 3 7) (Figure Pawn Black)
                            ,Tuple (Field 4 7) (Figure Pawn Black)
                            ,Tuple (Field 5 7) (Figure Pawn Black)
                            ,Tuple (Field 6 7) (Figure Pawn Black)
                            ,Tuple (Field 7 7) (Figure Pawn Black)
                            ,Tuple (Field 8 7) (Figure Pawn Black)
                            ,Tuple (Field 1 8) (Figure Rook Black)
                            ,Tuple (Field 2 8) (Figure Knight Black)
                            ,Tuple (Field 3 8) (Figure Bishop Black)
                            ,Tuple (Field 4 8) (Figure Queen Black)
                            ,Tuple (Field 5 8) (Figure King Black)
                            ,Tuple (Field 6 8) (Figure Bishop Black)
                            ,Tuple (Field 7 8) (Figure Knight Black)
                            ,Tuple (Field 8 8) (Figure Rook Black)
                            ]

{-
PS.Chess_Board.showBoard(PS.Chess_Board.gameStartBoard)
"
 abcdefgh
8RNBQKBNR8
7PPPPPPPP7
6........6
5........5
4........4
3........3
2pppppppp2
1rnbqkbnr1
 abcdefgh"

:i Chess.Board
showBoard gameStartBoard
-}
showBoard :: Board -> String
showBoard board =
  let showFieldContent row col = case M.lookup (Field col row) board of
                                   Just x -> showFigure x
                                   Nothing -> "."
      showRow row = (show row) ++
                    mconcat (map (showFieldContent row) (1..8)) ++
                    (show row) ++ "\n"
  in
    "\n abcdefgh\n" ++ mconcat (map showRow (8..1)) ++ " abcdefgh"

{-
PS.Chess_Board.showBoard(PS.Chess_Board.updateBoard(PS.Chess_Board.gameStartBoard)(PS.Chess_Move.RegularMove.create(PS.Chess_Field.Field.create(2)(2))(PS.Chess_Field.Field.create(2)(4))))
"
 abcdefgh
8RNBQKBNR8
7PPPPPPPP7
6........6
5........5
4.p......4
3........3
2p.pppppp2
1rnbqkbnr1
 abcdefgh"

:i Chess.Board
:i Chess.Move
:i Chess.Field
showBoard $ updateBoard gameStartBoard (RegularMove (Field 2 2) (Field 2 4))
-}
updateBoard :: Board -> Move -> Board
updateBoard board move = case move of
  RegularMove from to ->
    case M.lookup (from) board of
      Just figure -> M.insert to figure <<< M.delete from $ board
      _ -> board
  PromotionMove from to figure ->
    case M.lookup (from) board of
      Just _ -> M.insert to figure <<< M.delete (from) $ board
      _ -> board
  EnPassantMove from to captured ->
    case M.lookup (from) board of
      Just figure -> M.insert (to) figure <<<
                     M.delete (from) <<<
                     M.delete (captured) $ board
      _ -> board
  CastlingMove from to rookFrom rookTo ->
    case Tuple (M.lookup from board) (M.lookup rookFrom board) of
      Tuple (Just king) (Just rook) -> M.insert (to) king <<<
                                       M.insert (rookTo) rook <<<
                                       M.delete (from) <<<
                                       M.delete (rookFrom) $ board
      _ -> board

{-
-}
getFigure :: Board -> Field -> Maybe Figure
getFigure board field = M.lookup field board

getFigureUnicode :: Board -> Field -> String
getFigureUnicode board = maybe " " showFigureUnicode <<< getFigure board

{-
:i Chess.Board
:i Chess.Move
:i Chess.Field
showMove (RegularMove (Field 7 8) (Field 6 6)) gameStartBoard
"Ng8-f6 "
-}
showMove :: Move -> Board -> String
showMove move board = case move of
    RegularMove from to               -> mconcat [maybe " " showFigure' (getFigure board from), showField from, "-", showField to, " "]
    PromotionMove from to fig         -> mconcat [maybe " " showFigure' (getFigure board from), showField from, "-", showField to, showFigure fig]
    EnPassantMove from to _           -> mconcat [maybe " " showFigure' (getFigure board from), showField from, "-", showField to, " "]
    CastlingMove _ (Field col _) _ _  -> if col == 7 then "O-O    " else "O-O-O  "
  where
    showFigure' figure =
      let symbol = toUpper $ showFigure figure
      in if symbol == "P" then " " else symbol
