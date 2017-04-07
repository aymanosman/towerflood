{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment (getArgs)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import System.Random

newtype Tower = Tower (Height, Maybe Height)
  deriving (Show, Eq, Ord)

newtype Height = Height Int
  deriving (Num, Eq, Ord, Show)

main :: IO ()
main = do
    towerHeights <- fmap read <$> getArgs
    case towerHeights of
      [] -> genHeights 10 >>= run
      _ -> run towerHeights


genHeights :: Int -> IO [Int]
genHeights n =replicateM n (randomRIO (1,10))

run :: [Int] -> IO ()
run towerHeights = do
    let ts = fromHeights towerHeights
    for_ (fill ts) (putStrLn . show')

fill :: [Tower] -> [Tower]
fill ts =
  snd $ execState (replicateM (length ts) step) (0, ts)


step :: State (Int, [Tower]) ()
step = do
  (curr, ts) <- get
  let
    (leftMax, left) = maxLeft curr ts
    (rightMax, right) = maxRight curr ts
    Tower (amount, _) = min' leftMax rightMax
    t' = fillTower amount (ts !! curr)
    ts' = left ++ [t'] ++ right
  put (curr + 1, ts')
  where
    min' :: Maybe Tower -> Maybe Tower -> Tower
    min' leftMax rightMax =
      let
        l = fromMaybe empty leftMax
        r = fromMaybe empty rightMax
      in
          min l r

empty :: Tower
empty = Tower (0, Nothing)

fillTower :: Height -> Tower -> Tower
fillTower amount (Tower (h, _)) =
  Tower (h, Just (amount - h))

fromHeights :: [Int] -> [Tower]
fromHeights = map (\h -> Tower (Height h, Nothing))

maxLeft :: Ord a => Int -> [a] -> (Maybe a, [a])
maxLeft n ts =
  let
    left = take n ts
    leftMax =
      case left of
        [] -> Nothing
        _ -> Just $ maximum left
  in
    (leftMax, left)

maxRight :: Ord a => Int -> [a] -> (Maybe a, [a])
maxRight n ts =
  let
    right = drop (n + 1) ts
    rightMax =
      case right of
        [] -> Nothing
        _ -> Just $ maximum right
  in
    (rightMax, right)

show' :: Tower -> String
show' (Tower (Height height, mwater)) =
    let Height water = fromMaybe 0 mwater
    in
    replicate height '@' ++ replicate water '~'
