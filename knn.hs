{-# Language BangPatterns, FlexibleContexts #-}
import Prelude hiding (read, readFile, writeFile, lines, unlines)
import Text.Read (read)
import Data.Text.Lazy    (Text, lines, unlines, pack, unpack)
import Data.Text.Lazy.IO (readFile, writeFile)

import Text.Printf (printf)

import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Word

import Data.Vector (Vector)
import qualified Data.Vector as Vec (toList, fromList, foldl1', zipWith)

-----------------------------------------------------------------------------
-- GENERATE RECORDS FROM DATA
-----------------------------------------------------------------------------

newtype Label         =  Label (Maybe Word8)
            deriving (Eq, Ord)

newtype FeatureVector =  FeatureVector (Vector Word8)
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

              flatten :: [[Word8]] -> String
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

mkLabeledRecord :: [Word8] -> Record
mkLabeledRecord (x:xs) = Record (Label (Just x)) (FeatureVector (Vec.fromList xs))

mkUnlabeledRecord :: [Word8] -> Record
mkUnlabeledRecord xs = Record (Label Nothing) (FeatureVector (Vec.fromList xs))

classify :: [Record] -> Record -> Label
classify !trained !point = label (fst majority)
    where 
          majority = maximumBy (comparing snd) histogram

          histogram :: [(Record, Int)]
          histogram = [ (head xs, length xs) | xs <- groupBy ( (==) `on` label) (sortBy (comparing label) neighbors) ]
          neighbors :: [Record] 
          neighbors = let dist = comparing (distance point) in take k (sortBy dist  trained) 
          -- Number of nearest neighbors
          k = 100 :: Int

main :: IO ()
main = let readLine :: Text -> [Word8]
           readLine s = read ('[' : unpack s ++ "]")
       in
       do
       -- Load training sequence
       trainingInput <- readFile "data/train.csv"
       let trainingData = map (mkLabeledRecord . readLine) (drop 1 $ lines trainingInput)
       printf "Trainied using %d samples\n" (length trainingData)

       let classifier = classify trainingData

       testingInput <- readFile "data/sample.csv"
       let testingData = map (mkUnlabeledRecord . readLine) (drop 1 $ lines testingInput)
       let labels      = map classifier testingData
       let output      = map (pack.show) labels
       writeFile "out.csv" (unlines output)
