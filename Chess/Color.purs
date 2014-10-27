module Chess.Color where

data Color = White | Black

instance showColor :: Show Color where
  show White = "White"
  show Black = "Black"

instance eqColor :: Eq Color where
  (==) White White = true
  (==) Black Black = true
  (==) _ _ = false
  (/=) a b = not (a == b)

{-
PS.Chess_Color.other(PS.Chess_Color.White.value)
Black {}
-}
other :: Color -> Color
other White = Black
other Black = White

{-
PS.Chess_Color.firstRow(PS.Chess_Color.White.value)
1
-}
firstRow :: Color -> Number
firstRow White = 1
firstRow Black = 8
