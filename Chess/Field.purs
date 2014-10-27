module Chess.Field where

import Chess.Color
import Data.String


{-
PS.Chess_Field.Field.create(2)(4)
Field {value0: 2, value1: 4}
-}
data Field = Field Number -- column
                   Number -- row

instance fieldEq :: Eq Field where
  (==) (Field c1 r1) (Field c2 r2) = c1 == c2 && r1 == r2
  (/=) a b = not (a == b)

instance fieldOrd :: Ord Field where
  compare (Field c1 r1) (Field c2 r2) = if (c1 > c2) then GT
                                        else if c1 < c2 then LT
                                        else if r1 > r2 then GT
                                        else if r1 < r2 then LT
                                        else EQ

row (Field c r) = r
col (Field c r) = c

{-|
PS.Chess_Field.showField(PS.Chess_Field.Field.create(2)(4))
"b4"
-}
showField (Field c r) = fromCharCode (96 + c) ++ fromCharCode (48 + r)

{-
PS.Chess_Field.isLastRow(PS.Chess_Field.Field.create(2)(8))(PS.Chess_Color.White.value)
true
-}
isLastRow :: Field -> Color -> Boolean
isLastRow (Field _ r) color = r == firstRow (other color)

{-
PS.Chess_Field.isValid(PS.Chess_Field.Field.create(2)(8))
true
PS.Chess_Field.isValid(PS.Chess_Field.Field.create(2)(9))
false
-}
isValid :: Field -> Boolean
isValid (Field c r) = c >= 1 && c <= 8 && r >= 1 && r <= 8
