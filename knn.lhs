% Digit recognizer using K-nearest neighbors
% Aditya Mahajan
% 9 March 2013

This is a naive solution to the Kaggle problem on
[Digit Recognizer](http://www.kaggle.com/c/digit-recognizer)

Since the files are big, I used Lazy IO so tha tthe data could be processed in a
streaming manner. 

> import Prelude hiding (readFile, lines, unlines)
> import Data.ByteString.Lazy (ByteString, readFile)
> import Data.ByteString.Lazy.Char8 (lines)
> import System.IO (hPrint, openFile, IOMode(..), hSetBuffering, BufferMode(..), hClose)
> import Text.Printf (printf)

To parse each line of the file into records, I use a lazy Attoparsec parser.

> import Data.Attoparsec.ByteString.Lazy (Parser, sepBy1, (<?>), parse, maybeResult)
> import Data.Attoparsec.ByteString.Char8 (char, decimal) 

Load some generic helpful macros

> import Data.List (sortBy, groupBy, maximumBy)
> import Data.List.Split (chunksOf)
> import Data.Ord (comparing)
> import Data.Function (on)
> import Data.Maybe (fromJust)
> import Control.Arrow ( (&&&) )

Each pixel is an integer between 0 and 255, so store it as a Word8.

> import Data.Word (Word8)

Store the image as a Vector rather than a list.

> import Data.Vector.Storable (Vector)
> import qualified Data.Vector.Storable as Vec (sum, zipWith, fromList, toList)

Parallelism gives almost a factor of 4 speedup.

> import Control.Parallel.Strategies
> import Control.DeepSeq (NFData(..))

Data Structures
===============

Labels
------

> data Label = One
>            | Two
>            | Three
>            | Four
>            | Five
>            | Six
>            | Seven
>            | Eight
>            | Nine
>            | UnLabeled
>            deriving (Eq, Ord)
> 
> instance Show Label where
>       show One       = "1"
>       show Two       = "2"
>       show Three     = "3"
>       show Four      = "4"
>       show Five      = "5"
>       show Six       = "6"
>       show Seven     = "7"
>       show Eight     = "8"
>       show Nine      = "9"
>       show UnLabeled = "-"
> 
> instance NFData Label where
>       rnf a = a `seq` ()
> 
> toLabel :: Word8 -> Label
> toLabel = go where
>       go 1 = One      
>       go 2 = Two      
>       go 3 = Three    
>       go 4 = Four     
>       go 5 = Five     
>       go 6 = Six      
>       go 7 = Seven    
>       go 8 = Eight    
>       go 9 = Nine     
>       go _ = UnLabeled

Records
-------

A record is labeled or unlabeled feature vector
  
> data Record = Record Label (Vector Word8)
>
> instance Show Record where
>     show (Record l fv) = 
>         printf "Label: %s\n%s\n" (show l) (showVector fv)
>       where
>           showVector :: Vector Word8 -> String
>           showVector = flatten . normalize . chunksOf 28 . Vec.toList 
>
>           normalize = map (map (\a -> if a > 128 then 1 else 0))
> 
>           flatten :: [[Word8]] -> String
>           flatten = concatMap (\a -> concatMap show a ++ "\n") 


Get the label of a record

> label :: Record -> Label
> label (Record l _) = l 

Convert a list of Word8 into a labeled or unlabeled record. The function
`withLabels` is used while parsing training data while the function
`withoutLabels` is used while parsing testing data

> withLabels :: [Word8] -> Record
> withLabels ~(x:xs) = Record (toLabel x) (Vec.fromList xs)
> 
> withoutLabels :: [Word8] -> Record
> withoutLabels xs   = Record UnLabeled   (Vec.fromList xs)

Classification
==============

Eucledian distance
------------------

The k-NN algorithms relies on a distance between feature vectors. In this naive
implementation, we just use Eucledian distance, which is a really bad metric for
images. A better metic is to to IMED distance, which uses a Gaussian blurring
matrix as the weight matrix. 

In general, if we use a (positive definite) weight matrix $Q$, then $x^T Q x$
can be computed efficiently as follows:

- factorizing $Q = A^T A$
- scale the input as $x \mapsto A x$
- use Eucledian distance on the scaled image.

> {-# INLINE distance #-}
> distance :: Record -> Record -> Double
> distance (Record _ xs) (Record _ ys) = Vec.sum diff 
>     where diff = Vec.zipWith (\x y -> let d = fromIntegral (x-y) in d*d) xs ys

Cluster neighbors
----------------

Once the nearest neighbors have been identified, we need to cluster them
according to their labels.

> {-# INLINE cluster #-}
> cluster :: [(Label, Double)] -> [[(Label, Double)]]
> cluster = groupBy ( (==) `on` fst ) . sortBy (comparing fst)

k-nearest neighbors
--------------------

In addition to the choice of distance, there are two design choices in k-NN

- How many neighbors to pick?

> k :: Int
> k = 10 

- How much weight does each nearest neighbor contribute to its label

> {-# INLINE numOfNeighbors #-}
> numOfNeighbors :: [(Label, Double)] -> Double
> numOfNeighbors = fromIntegral . length

The final classification algorithm is pretty straight forward

> kNN :: [Record] -> Record -> Label
> kNN model p = fst . maximumBy (comparing snd) $! histogram
>   where
>         histogram :: [(Label, Double)]
>         histogram = map ( fst . head &&& numOfNeighbors ) -- Voting Algorithm
>                   . cluster $ neighbors
>
>         neighbors :: [(Label, Double)]
>         neighbors = take k sortedNeighbors
>         
>         sortedNeighbors :: [(Label, Double)]
>         sortedNeighbors = sortBy (comparing snd) distanceList `using`  parListChunk 1000 rdeepseq
>
>         distanceList :: [(Label, Double)]
>         distanceList = map (label &&& dist) model `using` parListChunk 1000 rdeepseq
>
>         dist :: Record -> Double
>         dist = distance p


File IO
=======

Parser to parse a record from file.

> record :: Parser [Word8]
> record = decimal `sepBy1` char ',' <?> "record"


Take a list of lines and convert them to nested list of `Word8`.

> vectorize :: [ByteString] -> [[Word8]]
> vectorize = map readLine
>     where  readLine :: ByteString -> [Word8]
>            readLine = fromJust . maybeResult . parse record

Use a labeling function to parse a file into records

> parseFile :: ([Word8] -> Record) -> FilePath -> IO [Record]
> parseFile lab fname = return . map lab . vectorize . tail . lines =<< readFile fname

Main
====

> main :: IO ()
> main = do
>        let trainingFile = "data/train.csv"
>            testingFile  = "data/sample.csv"
>            outputFile   = "data/output.csv"
>
>        -- Load sequences
>        trainingData <- parseFile withLabels    trainingFile
>        testingData  <- parseFile withoutLabels testingFile
> 
>        -- Classify
>        let classifier = kNN trainingData
>            labels     = map classifier testingData 
> 
>        -- Output result
>        printf "Trainied using %d samples\n" (length trainingData)
>        out <- openFile outputFile WriteMode
> 
>        -- Flush when the output has 200 bytes
>        hSetBuffering out (BlockBuffering (Just 200))
> 
>        sequence_ [ hPrint out l | l <- labels ]
>        hClose out
