module Chess.FigureMoves where

import Chess.Color
import Chess.Figure
import Chess.Field
import Data.Array
import Chess.Util

type Moves = { cols :: [Number], rows :: [Number] }

repeat :: forall a. Number -> a -> [a]
repeat 0 _ = []
repeat n elem = elem : repeat (n-1) elem 

{-
JSON.stringify(PS.Chess_FigureMoves.rookMoves)
"[{"cols":[1,2,3,4,5,6,7,8],"rows":[0,0,0,0,0,0,0,0]},{"cols":[-1,-2,-3,-4,-5,-6,-7,-8],"rows":[0,0,0,0,0,0,0,0]},{"cols":[0,0,0,0,0,0,0,0],"rows":[1,2,3,4,5,6,7,8]},{"cols":[0,0,0,0,0,0,0,0],"rows":[-1,-2,-3,-4,-5,-6,-7,-8]}]"
-}
rookMoves :: [Moves]
rookMoves = [{ cols : 1..8,       rows : repeat 8 0 }
            ,{ cols : -1..(-8),   rows : repeat 8 0 }
            ,{ cols : repeat 8 0, rows : 1..8 }
            ,{ cols : repeat 8 0, rows : -1..(-8) }
            ]

{-
JSON.stringify(PS.Chess_FigureMoves.bishopMoves)
"[{"cols":[1,2,3,4,5,6,7,8],"rows":[1,2,3,4,5,6,7,8]},{"cols":[-1,-2,-3,-4,-5,-6,-7,-8],"rows":[1,2,3,4,5,6,7,8]},{"cols":[1,2,3,4,5,6,7,8],"rows":[-1,-2,-3,-4,-5,-6,-7,-8]},{"cols":[-1,-2,-3,-4,-5,-6,-7,-8],"rows":[-1,-2,-3,-4,-5,-6,-7,-8]}]"
-}
bishopMoves :: [Moves]
bishopMoves = [{ cols : 1..8,     rows : 1..8 }
              ,{ cols : -1..(-8), rows : 1..8 }
              ,{ cols : 1..8,     rows : -1..(-8) }
              ,{ cols : -1..(-8), rows : -1..(-8) }
              ]

{-
JSON.stringify(PS.Chess_FigureMoves.queenMoves)
"[{"cols":[1,2,3,4,5,6,7,8],"rows":[0,0,0,0,0,0,0,0]},{"cols":[-1,-2,-3,-4,-5,-6,-7,-8],"rows":[0,0,0,0,0,0,0,0]},{"cols":[0,0,0,0,0,0,0,0],"rows":[1,2,3,4,5,6,7,8]},{"cols":[0,0,0,0,0,0,0,0],"rows":[-1,-2,-3,-4,-5,-6,-7,-8]},{"cols":[1,2,3,4,5,6,7,8],"rows":[1,2,3,4,5,6,7,8]},{"cols":[-1,-2,-3,-4,-5,-6,-7,-8],"rows":[1,2,3,4,5,6,7,8]},{"cols":[1,2,3,4,5,6,7,8],"rows":[-1,-2,-3,-4,-5,-6,-7,-8]},{"cols":[-1,-2,-3,-4,-5,-6,-7,-8],"rows":[-1,-2,-3,-4,-5,-6,-7,-8]}]"
-}
queenMoves :: [Moves]
queenMoves = rookMoves ++ bishopMoves

{-
JSON.stringify(PS.Chess_FigureMoves.knightMoves)
"[{"cols":[1],"rows":[2]},{"cols":[2],"rows":[1]},{"cols":[-1],"rows":[2]},{"cols":[2],"rows":[-1]},{"cols":[-1],"rows":[-2]},{"cols":[-2],"rows":[-1]},{"cols":[1],"rows":[-2]},{"cols":[-2],"rows":[1]}]"
-}
knightMoves :: [Moves]
knightMoves = [{ cols : [1],  rows : [2] }
              ,{ cols : [2],  rows : [1] }
              ,{ cols : [-1], rows : [2] }
              ,{ cols : [2],  rows : [-1] }
              ,{ cols : [-1], rows : [-2] }
              ,{ cols : [-2], rows : [-1] }
              ,{ cols : [1],  rows : [-2] }
              ,{ cols : [-2], rows : [1] }
              ]

{-
JSON.stringify(PS.Chess_FigureMoves.kingMoves)
"[{"cols":[1],"rows":[0]},{"cols":[-1],"rows":[0]},{"cols":[0],"rows":[1]},{"cols":[0],"rows":[-1]},{"cols":[1],"rows":[1]},{"cols":[-1],"rows":[1]},{"cols":[1],"rows":[-1]},{"cols":[-1],"rows":[-1]}]"
-}
kingMoves :: [Moves]
kingMoves = map (\{cols=c,rows=r} -> { cols : take 1 c, rows : take 1 r }) queenMoves

{-
JSON.stringify(PS.Chess_FigureMoves.kingMoves)
var whiteKing = PS.Chess_Figure.figure(PS.Chess_Figure.King.value)(PS.Chess_Color.White.value)
var whitePawn = PS.Chess_Figure.figure(PS.Chess_Figure.Pawn.value)(PS.Chess_Color.White.value)
var blackPawn = PS.Chess_Figure.figure(PS.Chess_Figure.Pawn.value)(PS.Chess_Color.Black.value)
JSON.stringify(PS.Chess_FigureMoves.chooseFigureMoves(whiteKing)(PS.Chess_Field.field(2)(2))(false))
"[{"cols":[1],"rows":[0]},{"cols":[-1],"rows":[0]},{"cols":[0],"rows":[1]},{"cols":[0],"rows":[-1]},{"cols":[1],"rows":[1]},{"cols":[-1],"rows":[1]},{"cols":[1],"rows":[-1]},{"cols":[-1],"rows":[-1]}]"
JSON.stringify(PS.Chess_FigureMoves.chooseFigureMoves(whitePawn)(PS.Chess_Field.field(1)(4))(false))
"[{"cols":[0],"rows":[1]}]"
JSON.stringify(PS.Chess_FigureMoves.chooseFigureMoves(blackPawn)(PS.Chess_Field.field(1)(7))(false))
"[{"cols":[0,0],"rows":[-1,-2]}]"
-}
chooseFigureMoves :: Figure -> Field -> Boolean -> [Moves]
chooseFigureMoves (Figure Rook _)                 _           _     = rookMoves
chooseFigureMoves (Figure Bishop _)               _           _     = bishopMoves
chooseFigureMoves (Figure King _)                 _           _     = kingMoves
chooseFigureMoves (Figure Queen _)                _           _     = queenMoves
chooseFigureMoves (Figure Knight _)               _           _     = knightMoves
chooseFigureMoves (Figure Pawn White) (Field _ 2) false = [{ cols : [0,0], rows : [1,2]}]
chooseFigureMoves (Figure Pawn White) _           false = [{ cols : [0], rows : [1]}]
chooseFigureMoves (Figure Pawn Black) (Field _ 7) false = [{ cols : [0,0], rows : [-1,-2]}]
chooseFigureMoves (Figure Pawn Black) _           false = [{ cols : [0], rows : [-1]}]
chooseFigureMoves (Figure Pawn White) _           true  = [{ cols : [-1], rows : [1]}
                                                          ,{ cols : [1],  rows : [1]}]
chooseFigureMoves (Figure Pawn Black) _           true  = [{ cols : [-1], rows : [-1]}
                                                          ,{ cols : [1],  rows : [-1]}]

type Delta = { dc :: Number, dr :: Number }

delta c r = { dc : c, dr : r }

{-
-}
relativeField :: Field -> Delta -> Field
relativeField (Field col row) {dc=dc,dr=dr} = Field (col+dc) (row+dr)

{-
-}
relativeFields :: Field -> Moves -> [Field]
relativeFields field {cols=cols,rows=rows} =
  takeWhile isValid (map (relativeField field) (zipWith delta cols rows))

{-
-}

{-
var whiteRook = PS.Chess_Figure.figure(PS.Chess_Figure.Rook.value)(PS.Chess_Color.White.value)
JSON.stringify(PS.Chess_FigureMoves.figureMoves(whiteRook)(PS.Chess_Field.field(2)(2))(false))
"[[{"col":3,"row":2},{"col":4,"row":2},{"col":5,"row":2},{"col":6,"row":2},{"col":7,"row":2},{"col":8,"row":2}],[{"col":1,"row":2}],[{"col":2,"row":3},{"col":2,"row":4},{"col":2,"row":5},{"col":2,"row":6},{"col":2,"row":7},{"col":2,"row":8}],[{"col":2,"row":1}]]"
-}
figureMoves :: Figure -> Field -> Boolean -> [[Field]]
figureMoves figure field capture = map (relativeFields field) $ chooseFigureMoves figure field capture
