import Data.Bits (Bits (xor))

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDisks peg1 peg2 peg3
  | numDisks >= 1 = 
  | numDisks <= 0 = 

toStack :: Integer -> [Integer]
toStack x
  | x < 1 = []
  | otherwise = x : toStack (x - 1)


