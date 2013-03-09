{-# Language OverloadedStrings, BangPatterns, FlexibleContexts #-}

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
import Control.DeepSeq (NFData(..))

-- Data structures for storing data
import Data.Word (Word8)

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec (sum, zipWith, fromList, toList)

record :: Parser [Word8]
record = decimal `sepBy1` char ',' <?> "record"

data Label = One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | UnLabeled
           deriving (Eq, Ord)

instance Show Label where
      show One       = "1"
      show Two       = "2"
      show Three     = "3"
      show Four      = "4"
      show Five      = "5"
      show Six       = "6"
      show Seven     = "7"
      show Eight     = "8"
      show Nine      = "9"
      show UnLabeled = "-"

instance NFData Label where
      rnf a = a `seq` ()

toLabel :: Word8 -> Label
toLabel = go where
      go 1 = One      
      go 2 = Two      
      go 3 = Three    
      go 4 = Four     
      go 5 = Five     
      go 6 = Six      
      go 7 = Seven    
      go 8 = Eight    
      go 9 = Nine     
      go _ = UnLabeled
  
data Record = Record !Label (Vector Word8)

withLabels :: [Word8] -> Record
withLabels (x:xs) = Record (toLabel x) (Vec.fromList xs)

withoutLabels :: [Word8] -> Record
withoutLabels xs   = Record UnLabeled   (Vec.fromList xs)

showVector :: Vector Word8 -> String
showVector vec = flatten . normalize . grid $ Vec.toList vec
    where grid [] = []
          grid y = let (a,as) = splitAt 28 y in a:grid as
                    
          normalize = map (map (\a -> if a > 128 then 1 else 0))

          flatten :: [[Word8]] -> String
          flatten = concatMap (\a -> concatMap show a ++ "\n") 

instance Show Record where
    show (Record l fv) = 
        printf "Label: %s\n%s\n" (show l) (showVector fv)

label :: Record -> Label
label (Record l _) = l 

-- Eucledian distance is not a good metric for distance between images
-- Use IMED distance instead, which uses a Gaussian weight matrix.
--
-- If we are using Eucledian distance, then x^T Q x can be computed efficiently
-- by first factorizing Q as A^T A. Then we can simply do a coordinate transform
-- x -> Ax and do the usual Eucledian distance calculations.
distance :: Record -> Record -> Double
distance (Record _ xs) (Record _ ys) = Vec.sum diff 
    where diff = Vec.zipWith (\x y -> let d = fromIntegral (x-y) in d*d) xs ys

{-# INLINE distance #-}

classify :: [Record] -> Record -> Label
classify !model !point = fst majority
    where 
          majority = maximumBy (comparing snd) histogram

          histogram :: [(Label, Double)]
          histogram =  map (fst . head &&& weight) 
                    .  groupBy ( (==) `on` fst )
                    .  sortBy  (comparing fst )
                    $  neighbors

          weight :: [(Label, Double)] -> Double
          weight = fromIntegral . length

          -- The naive implementation is slow, possibly because dist is being
          -- called O(n*lon(n)) times rather than O(n) times.
          -- neighbors :: [Record] 
          -- neighbors = let dist = comparing (distance point) in take k (sortBy dist  model) 
          dist      :: [(Label, Double)]
          dist      =  map (label &&& distance point) model `using` parListChunk 1000 rdeepseq

          neighbors :: [(Label, Double)]
          neighbors = take k ((sortBy (comparing snd) dist) `using` parListChunk 1000 rdeepseq)
          
          -- Number of nearest neighbors
          k = 10 :: Int

vectorize :: [ByteString] -> [[Word8]]
vectorize = map readLine
    where  readLine :: ByteString -> [Word8]
           readLine = fromJust . maybeResult . parse record

parseFile :: ([Word8] -> Record) -> FilePath -> IO [Record]
parseFile lab fname = return . map lab . vectorize . tail . lines =<< readFile fname


main :: IO ()
main = do
       -- Load sequences
       trainingData <- parseFile withLabels    "data/train.csv" 
       testingData  <- parseFile withoutLabels "data/sample.csv"

       -- Classify
       let classifier = classify trainingData
           labels     = map classifier testingData 

       -- Output result
       printf "Trainied using %d samples\n" (length trainingData)
       out <- openFile "data/output.csv" WriteMode     

       -- Flush when the output has 200 bytes
       hSetBuffering out (BlockBuffering (Just 200))

       sequence_ [ hPrint out l | l <- labels ]
       hClose out
