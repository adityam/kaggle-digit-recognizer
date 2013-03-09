{-# Language OverloadedStrings, BangPatterns, FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}

-- K-nearest neighbors based solution for
-- http://www.kaggle.com/c/digit-recognizer
--
-- Author: Aditya Mahajan 

-- Use Lazy IO instead of default IO.
import Prelude hiding (readFile, writeFile, lines, unlines)
import Data.ByteString.Lazy (ByteString, readFile)
import Data.ByteString.Lazy.Char8 (lines)

-- File IO
import System.IO (hPrint, openFile, IOMode(..), hSetBuffering, BufferMode(..), hClose)

-- Parser for reading CSV files
import Data.Attoparsec.ByteString.Lazy (Parser, sepBy1, (<?>), parse, maybeResult)
import Data.Attoparsec.ByteString.Char8 (char, decimal) 

import Text.Printf (printf)

-- Useful generic functions
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (fromJust)
import Control.Arrow ( (&&&) )

import Control.Parallel.Strategies

-- Data structures for storing data
import Data.Word (Word8)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec (sum, zipWith, fromList, toList)

record :: Parser [Word8]
record = decimal `sepBy1` char ',' <?> "record"

newtype Label         =  Label (Maybe Word8)
            deriving (Eq, Ord, NFData)

newtype FeatureVector =  FeatureVector (Vector Word8)

data Record = Record !Label !FeatureVector

mkLabeledRecord :: [Word8] -> Record
mkLabeledRecord (x:xs) = Record (Label (Just x)) (FeatureVector (Vec.fromList xs))

mkUnlabeledRecord :: [Word8] -> Record
mkUnlabeledRecord xs   = Record (Label Nothing)  (FeatureVector (Vec.fromList xs))

instance Show Label where
   show (Label l) = case l of 
        Just d  -> show d
        Nothing -> "-"

instance Show FeatureVector where
    show (FeatureVector vec) = flatten . normalize . grid $ Vec.toList vec
        where grid [] = []
              grid y = let (a,as) = splitAt 28 y in a:grid as
                        
              normalize = map (map (\a -> if a > 128 then 1 else 0))

              flatten :: [[Word8]] -> String
              flatten = concatMap (\a -> concatMap show a ++ "\n") 

instance Show Record where
    show (Record l fv) = 
        printf "Label: %s\n%s\n" (show l) (show fv)

label :: Record -> Label
label (Record l _) = l 

-- Eucledian distance is not a good metric for distance between images
-- Use IMED distance instead, which uses a Gaussian weight matrix.
--
-- If we are using Eucledian distance, then x^T Q x can be computed efficiently
-- by first factorizing Q as A^T A. Then we can simply do a coordinate transform
-- x -> Ax and do the usual Eucledian distance calculations.
distance :: Record -> Record -> Double
distance (Record _ (FeatureVector xs)) (Record _ (FeatureVector ys)) = Vec.sum diff 
    where diff = Vec.zipWith (\x y -> let d = fromIntegral (x-y) in d*d) xs ys

{-# INLINE distance #-}

classify :: [Record] -> Record -> Label
classify !model !point = (fst majority)
    where 
          majority = maximumBy (comparing snd) histogram

          histogram :: [(Label, Int)]
          histogram = [ (fst (head xs), length xs) 
                      | xs <- groupBy ( (==) `on` fst) (sortBy (comparing fst) neighbors) ]

          -- The naive implementation is slow, possibly because dist is being
          -- called O(n*lon(n)) times rather than O(n) times.
          -- neighbors :: [Record] 
          -- neighbors = let dist = comparing (distance point) in take k (sortBy dist  model) 
          dist      :: [(Label, Double)]
          dist      =  map (label &&& distance point) model `using` parListChunk 1000 rdeepseq

          neighbors :: [(Label, Double)]
          neighbors = take k ( (sortBy (comparing snd) dist) `using` parListChunk 1000 rdeepseq)
          
          -- Number of nearest neighbors
          k = 10 :: Int

main :: IO ()
main = let readLine :: ByteString -> [Word8]
           readLine = fromJust . maybeResult . parse record

           parseFile :: String -> IO [ByteString]
           parseFile = fmap (tail . lines) . readFile
       in
       do
       -- Load training sequences
       trainingInput <- parseFile "data/train.csv"
       let trainingData = map (mkLabeledRecord . readLine) trainingInput

       -- Load testing sequences
       testingInput <- parseFile "data/test.csv"
       let testingData = map (mkUnlabeledRecord . readLine) testingInput

       -- Classify
       let classifier = classify trainingData
           labels      = map classifier testingData 

       -- Output result
       printf "Trainied using %d samples\n" (length trainingData)
       out <- openFile "data/output.csv" WriteMode     

       -- Flush when the output has 100 bytes
       hSetBuffering out (BlockBuffering (Just 200))

       sequence_ [ hPrint out l | l <- labels ]
       hClose out
