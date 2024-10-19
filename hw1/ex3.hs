type Peg = String

type Move = (Peg, Peg)

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour n a b c d
  | n <= 0 = [(a, b)]
  | otherwise = hanoiFour (n - 1) a d b c ++ hanoiFour (n - 2) a c b d ++ [(a, b)] ++ hanoiFour (n - 2) c b d a ++ hanoiFour (n - 1) d b c a

main = do
  print (hanoiFour 2 "a" "b" "c" "d")
