import System.Environment (getArgs)
import Control.Monad.State
import Data.Foldable (for_)
import System.Random

main :: IO ()
main = do
    towerHeights <- fmap read <$> getArgs
    case towerHeights of
      [] -> genHeights 10 >>= run
      _ -> run towerHeights

run :: [Int] -> IO ()
run towerHeights =
    for_ (ans towerHeights) (putStrLn . showTower)

genHeights :: Int -> IO [Int]
genHeights n =replicateM n (randomRIO (1,10))

maxSoFar :: Int -> [Int] -> [Int]
maxSoFar _ [] = []
maxSoFar n (x : xs) =
  let m = max n x
  in m : maxSoFar m xs

maxPre :: [Int] -> [Int]
maxPre = maxSoFar 0

inReverse :: ([a] -> [b]) -> [a] -> [b]
inReverse f xs = reverse (f (reverse xs))

maxSuf :: [Int] -> [Int]
maxSuf = inReverse (maxSoFar 0)

ans :: [Int] -> [(Int, Int)]
ans ls =  [ (v, min left right - v) |
            (v, left, right) <- zip3 ls (maxPre ls) (maxSuf ls) ]

showTower :: (Int, Int) -> String
showTower (height, water) =
  replicate height '@' ++ replicate water '~'
