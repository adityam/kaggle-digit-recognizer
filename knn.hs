{-# Language BangPatterns, FlexibleContexts #-}
import Text.ParserCombinators.Parsec 
        ( endBy, sepBy
        , skipMany, many1
        , noneOf,  oneOf
        , char
        , parse, ParseError
        , parseFromFile
        )
import Text.Parsec (spaces, digit, Stream, ParsecT)
import Text.Printf (printf)

import Data.Char (digitToInt)
import Data.List (foldl1')

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec (toList, fromList, foldl1', zipWith)

import Data.Trees.KdTree (KdTree, Point(..))
import qualified Data.Trees.KdTree as NN (fromList, kNearestNeighbors)

-----------------------------------------------------------------------------
-- PARSE DATA
-----------------------------------------------------------------------------
-- Simple parser from http://book.realworldhaskell.org/read/using-parsec.html
-- This assumes that each line is a string of int separated by comma.
-- Does not accept spaces between digit and comma
csv = do { firstLine; endBy line eol }
    where 
          -- Ignore the first line
          firstLine = skipMany (noneOf "\n")

          eol  = many1 (oneOf "\r\n")

          line :: Stream s m Char => ParsecT s u m [Int]
          line = sepBy digits (char ',')

          -- Digit parser from http://stackoverflow.com/a/10726784/193149
          digits :: Stream s m Char => ParsecT s u m Int
          digits = digitWithSpace

          digitWithSpace :: Stream s m Char => ParsecT s u m Int
          digitWithSpace = do
                              spaces
                              s <- many1 digit
                              let d = strToInt s
                              return d

          strToInt :: String -> Int
          strToInt = foldl1 (\a i -> a*10 + i) .  map digitToInt 

parseCSV :: String -> Either ParseError [[Int]]
parseCSV = parse csv "(unknown)" 

parseCSVFromFile :: String -> IO (Either ParseError [[Int]])
parseCSVFromFile = parseFromFile csv

-----------------------------------------------------------------------------
-- GENERATE RECORDS FROM DATA
-----------------------------------------------------------------------------

data Record = Record { labelData :: Maybe Int,
                       rawData   :: Vector Int }
              deriving Eq

instance Show Record where
    show (Record l xs) = 
        let label = case l of 
              Just d  -> show d
              Nothing -> "-"
        in 
        printf "Label: %s\n%s\n" label ( flatten . normalize . grid $ Vec.toList xs)
        where grid [] = []
              grid y = let (a,as) = splitAt 28 y in a:grid as
                        
              normalize = map (map (\a -> if a > 128 then 1 else 0))

              flatten :: [[Int]] -> String
              flatten = concatMap (\a -> concatMap show a ++ "\n") 
          
instance Point Record where
    dimension _ = 784 -- 28*28
    coord i (Record _ xs)   = fromIntegral (xs!i)
    dist2 (Record _ xs) (Record _ ys) = Vec.foldl1' (+) diff 
            where diff = Vec.zipWith (\x y -> fromIntegral (x-y)^2) xs ys

mkLabeledRecord :: [Int] -> Record
mkLabeledRecord (x:xs) = Record (Just x) (Vec.fromList xs)

mkUnlabeledRecord :: [Int] -> Record
mkUnlabeledRecord xs = Record Nothing (Vec.fromList xs)

train :: [[Int]] -> KdTree Record
train d = NN.fromList records 
    where records :: [Record]
          records = map mkLabeledRecord d

classify tree d = map (classifyRecord tree) records
    where records :: [Record]
          records = map mkUnlabeledRecord d

classifyRecord tree d = fmap (\t -> NN.kNearestNeighbors t 10 d) tree

main = do
       -- Load training sequence
       input <- parseCSVFromFile "train-sample.csv"
       let trainedData  = fmap train input 
       input <- parseCSVFromFile "sample.csv"
       let testData = fmap (classify trainedData) input
       print testData

