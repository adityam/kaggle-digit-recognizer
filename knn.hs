{-# Language FlexibleContexts #-}
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
import Data.List (foldl1', sortBy, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)

import Control.Applicative (liftA2)

import Data.Vector (Vector)
import qualified Data.Vector as Vec (toList, fromList, foldl1', zipWith)

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
          strToInt = foldl1' (\a i -> a*10 + i) .  map digitToInt 

parseCSV :: String -> Either ParseError [[Int]]
parseCSV = parse csv "(unknown)" 

parseCSVFromFile :: String -> IO (Either ParseError [[Int]])
parseCSVFromFile = parseFromFile csv

-----------------------------------------------------------------------------
-- GENERATE RECORDS FROM DATA
-----------------------------------------------------------------------------

newtype Label         =  Label (Maybe Int)
            deriving (Eq, Ord)

newtype FeatureVector =  FeatureVector (Vector Int)
            deriving (Eq)

instance Show Label where
   show (Label l) = case l of 
        Just d  -> show d
        Nothing -> "-"

instance Show FeatureVector where
    show (FeatureVector vec) = flatten . normalize . grid . Vec.toList $ vec
        where grid [] = []
              grid y = let (a,as) = splitAt 28 y in a:grid as
                        
              normalize = map (map (\a -> if a > 128 then 1 else 0))

              flatten :: [[Int]] -> String
              flatten = concatMap (\a -> concatMap show a ++ "\n") 

data Record = Record !Label !FeatureVector
            deriving (Eq)

label :: Record -> Label
label (Record l _) = l 

instance Show Record where
    show (Record l fv) = 
        printf "Label: %s\n%s\n" (show l) (show fv)

-- Eucledian distance is not a good metric for distance between images
-- Use IMED distance instead
distance :: Record -> Record -> Double
distance (Record _ (FeatureVector xs)) (Record _ (FeatureVector ys)) = Vec.foldl1' (+) diff 
    where diff = Vec.zipWith (\x y -> fromIntegral (x-y)^2) xs ys

mkLabeledRecord :: [Int] -> Record
mkLabeledRecord (x:xs) = Record (Label (Just x)) (FeatureVector (Vec.fromList xs))

mkUnlabeledRecord :: [Int] -> Record
mkUnlabeledRecord xs = Record (Label Nothing) (FeatureVector (Vec.fromList xs))

train :: [[Int]] -> [Record]
train d = records 
    where records :: [Record]
          records = map mkLabeledRecord d

classify :: [Record] -> [[Int]] -> [(Record, Int)]
classify labeled d = map (classifyRecord labeled) records
    where records :: [Record]
          records = map mkUnlabeledRecord d

classifyRecord :: [Record] -> Record -> (Record, Int)
classifyRecord labeled point = majority -- label (fst majority)
    where 
          majority = maximumBy (comparing snd) histogram

          histogram :: [(Record, Int)]
          histogram = [ (head xs, length xs) | xs <- groupBy ( (==) `on` label) (sortBy (comparing label) neighbors) ]
          neighbors :: [Record] 
          neighbors = let dist = comparing (distance point) in take k (sortBy dist  labeled) 
          -- Number of nearest neighbors
          k = 10 :: Int

main :: IO ()
main = do
       -- Load training sequence
       input <- parseCSVFromFile "train-sample.csv"
       let trainedData  = fmap train input 
       -- This has to be done in a lazy manner
       input <- parseCSVFromFile "sample.csv"
       let testData = liftA2 classify trainedData input
       print testData

