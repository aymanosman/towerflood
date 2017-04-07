import Data.Vect

ls : List Nat
ls = [8,3,5,10,4,6,1,9]

maxSoFar : Nat -> List Nat -> List Nat
maxSoFar _ [] = []
maxSoFar n (x :: xs) =
  let m = max n x
  in m :: maxSoFar m xs

maxPre : List Nat -> List Nat
maxPre = maxSoFar 0

inReverse : (List a -> List b) -> List a -> List b
inReverse f xs = reverse (f (reverse xs))

maxSuf : List Nat -> List Nat
maxSuf = inReverse (maxSoFar 0)

ans : List (Nat, Int)
ans =  [ (v, cast (min left right) - cast v) |
         (v, left, right) <- zip3 ls (maxPre ls) (maxSuf ls) ]

showTower : Nat -> (Nat, Int) -> List Char
showTower pad (height, water) =
  let
    tower = replicate height '@'
    water' = replicate (cast water) '~'
    padding = cast pad - (cast height + water)
  in
  tower ++ water' ++ (replicate (cast padding) ' ')

max' : List Nat -> Nat
max' ls = foldl maximum 0 ls

main : IO ()
main =
  let
    max = foldl maximum 0 ls
    ts = map (showTower max) ans
    ts' = map pack $ transpose (map reverse ts)
  in
  for_ ts' putStrLn
