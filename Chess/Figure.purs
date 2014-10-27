module Chess.Figure where

import Data.String
import Chess.Color

data FigureType = King | Queen | Rook | Bishop | Knight | Pawn

instance eqFigureType :: Eq FigureType where
  (==) King King = true
  (==) Queen Queen = true
  (==) Rook Rook = true
  (==) Bishop Bishop = true
  (==) Knight Knight = true
  (==) Pawn Pawn = true
  (==) _ _ = false
  (/=) a b = not (a == b)

data Figure = Figure FigureType Color

instance eqFigure :: Eq Figure where
  (==) (Figure t1 c1) (Figure t2 c2) = t1 == t2 && c1 == c2
  (/=) a b = not (a == b)

figureColor :: Figure -> Color
figureColor (Figure _ c) = c

figureType :: Figure -> FigureType
figureType (Figure t _) = t

{-
PS.Chess_Figure.showFigure(PS.Chess_Figure.figure(PS.Chess_Figure.King.value)(PS.Chess_Color.White.value))
"k"
-}
showFigure :: Figure -> String
showFigure (Figure King   White) = "k"
showFigure (Figure Queen  White) = "q"
showFigure (Figure Rook   White) = "r"
showFigure (Figure Bishop White) = "b"
showFigure (Figure Knight White) = "n"
showFigure (Figure Pawn   White) = "p"
showFigure (Figure King   Black) = "K"
showFigure (Figure Queen  Black) = "Q"
showFigure (Figure Rook   Black) = "R"
showFigure (Figure Bishop Black) = "B"
showFigure (Figure Knight Black) = "N"
showFigure (Figure Pawn   Black) = "P"

{-
PS.Chess_Figure.showFigureUnicode(PS.Chess_Figure.figure(PS.Chess_Figure.King.value)(PS.Chess_Color.White.value))
-}
showFigureUnicode :: Figure -> String
showFigureUnicode (Figure King   White) = fromCharCode(9812)
showFigureUnicode (Figure Queen  White) = fromCharCode(9813)
showFigureUnicode (Figure Rook   White) = fromCharCode(9814)
showFigureUnicode (Figure Bishop White) = fromCharCode(9815)
showFigureUnicode (Figure Knight White) = fromCharCode(9816)
showFigureUnicode (Figure Pawn   White) = fromCharCode(9817)
showFigureUnicode (Figure King   Black) = fromCharCode(9818)
showFigureUnicode (Figure Queen  Black) = fromCharCode(9819)
showFigureUnicode (Figure Rook   Black) = fromCharCode(9820)
showFigureUnicode (Figure Bishop Black) = fromCharCode(9821)
showFigureUnicode (Figure Knight Black) = fromCharCode(9822)
showFigureUnicode (Figure Pawn   Black) = fromCharCode(9823)
