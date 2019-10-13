{-# LANGUAGE ParallelListComp, ScopedTypeVariables #-}

-- Something like Cocke-Younger-Kasami CFG parsing algorithm in Haskell
--   * No Chomsky Normal Form required, but Epsilons are not allowed.
--   * Not optimized.

module Main where

import Data.Either
import Data.List
import Data.Array.IArray as A
import Control.Arrow (second)
import Control.Monad.Except

-- Partition a number n into exactly l parts of sizes \in \{1..f\}
-- 1st arg: the number to be partitioned
-- 2nd arg: = no. of elts in a partitioning
-- 3rd arg: max size of a single element of a partitioning
posPartitionsL :: (Integral a) => a -> a -> a -> [[a]]
posPartitionsL 0 _ _ = []
posPartitionsL _ 0 _ = [[]]
posPartitionsL n 1 f | f < n = []
posPartitionsL n 1 _ = [[n]]
posPartitionsL n l f =
  concat [ map (k:) (posPartitionsL (n-k) (l-1) f) | k <- [1..min n f] ]

listcorrespondences :: Eq a => [[a]] -> [[a]] -> [[a]]
listcorrespondences as bs = zipWith (Data.List.intersect) as bs

-- Epsilon excluded.
data CFG a =
  CFG { numnt :: Integer,
        rules :: [(Integer, Either [Integer] [a])] }
  deriving (Show)

cykparse :: (Eq a) => CFG a -> [a] -> Except [Char] [Integer]
cykparse cfg w = do
  c <- cyk cfg w
  return $ fst $ (A.!) c (length w, 1)

-- A list of unique nonterminals and a detailed resolution of the ambiguity
type CykStruct = Array (Int, Int) ([Integer], [(Integer, [(Int, Int)])])

cyk :: Eq a => CFG a -> [a] -> Except [Char] CykStruct
cyk _ [] = throwError "CYK called on empty string"
cyk cfg (h:t) =
  let
    crules = [(x',i) | (i,x) <- rules cfg, isRight x, let x' = fromRight [] x]
    n      = length (h:t)
    arr    = A.array ((1,1), (n,n)) entries
    entries = [ ((1,i), (suitablents, map (\x -> (x,[])) suitablents))
              | i <- [1..n] | c <- (h:t)
              , let suitablents =  [j | (x',j) <- crules, c`elem`x' ] ]
  in
    return $ cykparse' cfg arr n 2

-- now part of library
-- fromRight :: b -> Either a b -> b
-- fromRight _ (Right x)  = x
-- fromRight d (Left _) = d
-- fromLeft :: a -> Either a b -> a
-- fromLeft _ (Left x)  = x
-- fromLeft d (Right _) = d

cykparse' :: CFG a -> CykStruct
     -> Int -> Int -> CykStruct
cykparse' _ a n j | n < j = a
cykparse' cfg a n j =
  let
    syntacticRules = [(x',i) | (i,x) <- rules cfg, isLeft x, let x' = fromLeft [] x]
    a' = a // [ ((j,i),
                 (nub $ map fst contents, contents))
              | i <- [1..n-j+1]
              , let contents = [ (nt, origins)
                               | (x',nt) <- syntacticRules
                               , length x' <= j
                               , partit  <- posPartitionsL j (length x') (j-1)
                               , let origins = map (second (+i)) (zip partit (scanl (+) 0 partit))
                               , let ntsequence :: [[Integer]] = map (\(k,s') -> fst ((A.!) a (k,s'))) origins
                               , let intersects :: [[Integer]] = (listcorrespondences (map return x') ntsequence)
                               , any (const True) intersects && not (any null intersects) ]
              ]
           // [ ((j,i), ([],[])) | i <- [n-j+2..n] ]
  in
    cykparse' cfg a' n (j+1)

-- Example: Dyck_2 grammar
dyckab :: CFG Char
dyckab = CFG 3 [(0, Left [0,0]), (0, Left [1,2]), (0, Left [1,0,2]), (1, Right "a"), (2, Right "b")]

main :: IO ()
main = do
    putStrLn "~ Ready ~"
    sequence
      [
      do
        s <- getLine
        let g = dyckab
        let result = runExcept (cykparse g s)
        case result of
          Right x  -> putStrLn (show x)
          Left y -> putStrLn ("Error: " ++ y)
      | _ <- [1..]]
    return ()
