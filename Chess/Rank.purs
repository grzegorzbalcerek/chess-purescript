module Chess.Rank where

import qualified Data.Map as M
import Data.Array
import Data.Foldable
import Data.Tuple
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Chess.FigureMoves
import Chess.Game
import Chess.Color

{-
PS.Chess_Rank.rank(PS.Chess_Game.GameStart.value)(PS.Chess_Color.White.value)
10
PS.Chess_Rank.rank(PS.Chess_Game.GameStart.value)(PS.Chess_Color.Black.value)
-10
PS.Chess_Rank.colorRank(PS.Chess_Game.GameStart.value)(PS.Chess_Color.Black.value)
3920
-}

figureRank :: Figure -> Number
figureRank (Figure figureType _) = case figureType of
  Queen -> 900
  Rook -> 450
  Knight -> 300
  Bishop -> 300
  Pawn -> 100
  _ -> 0

fieldRank :: Field -> Number
fieldRank (Field col row) =
  let colRowRank cr = if cr>=5 then 9-cr else cr
  in 2*colRowRank(col) * colRowRank(row)

figureDefendingOtherFiguresRank :: Game -> Field -> Figure -> Number
figureDefendingOtherFiguresRank game field figure =
  (length $ defendedDestinations game (figureMoves figure field true)) / 2

checkRank :: Game -> Color -> Number
checkRank game color =
  if (gameColor game == other color) && isKingUnderCheck game then 50 else 0

colorRank :: Game -> Color -> Number
colorRank game color =
  let ranks =
        (M.toList $ gameBoard game) #
        filter (\(Tuple _ figure) -> figureColor figure == color) #
        map (\(Tuple field figure) ->
          let r1 = figureRank figure
              r2 = fieldRank field
              r3 = figureDefendingOtherFiguresRank game field figure
          in r1+r2+r3)
  in sum ranks + checkRank game color

rank :: Game -> Color -> Number
rank game color = colorRank game color - colorRank game (other color)
