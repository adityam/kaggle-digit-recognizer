% Average number of correct entries between two lists
% Aditya Mahajan
% 9 March, 2013

> {-# LANGUAGE BangPatterns #-}

This program calculates the average number of entries between two lists, each
list is specified in a text file. This is a helper program for the Kaggle
problem on [Digit Recognizer](http://www.kaggle.com/c/digit-recognizer/).

The desired output format for that problem looks like:

    3
    7
    8
    (27997 more lines)

When trying different algorithms, it is useful to check the difference between
two solutions. This program simply computes $∑_i δ(x_i-y_i)$, where $δ$ is the
Kronecker delta function.

Since the files are big, I used Lazy IO so tha tthe data could be processed in a
streaming manner. 

> import Prelude hiding (readFile, lines, unlines)
> import Data.ByteString.Lazy (ByteString, readFile)
> import Data.ByteString.Lazy.Char8 (lines)

To parse each line of the file into digits, I use a lazy Attoparsec parser.

> import Data.Attoparsec.ByteString.Lazy  (parse, maybeResult)
> import Data.Attoparsec.ByteString.Char8 (decimal)

By default, the Attoparsec parser results its own data structure. To simply the
code, I assume that the file is always in the correct format (so that parsing is
always done correctly). So, I parse using:

    fromJust . maybeResult . parse

> import Data.Maybe (fromJust)
> import Data.List (foldl')

The usage of this program is:

    error-rate file1 file2

The next import handles the program arguments

> import System.Environment (getArgs)


Helper functions
================

Vectorize
---------

This function takes a sequence of lines (each line is assumed to a string
containing a single digit) and converts it to a list of Int

> vectorize :: [ByteString] -> [Int]
> vectorize = map readLine 
>      where
>            readLine :: ByteString -> Int
>            readLine = fromJust . maybeResult . parse decimal

Match
-----

This function computes the percentage match between two lists. For speed, the
length is calculated using an accumulator, so that the elements of the list can
be consumed as soon as possible.

> match :: [Int] -> [Int] -> Double
> match xs ys = (fromIntegral difference / fromIntegral size)
>       where delta :: Int -> Int -> Int
>             delta !x !y = if x == y then 1 else 0
>
>             matches :: [Int] 
>             matches = zipWith delta xs ys           
>
>             acc :: (Int, Int) -> Int -> (Int, Int)
>             acc (!x, !y) z = (x+z, y+1)
>
>             (difference, size) = foldl' acc (0,0) matches

Parsefile
---------

This function lazily reads a file and returns it as a list of Int

> parseFile :: FilePath -> IO [Int]
> parseFile fname = readFile fname >>= return . vectorize . lines 

Main function
=============

> main :: IO ()
> main = do
>   args <- getArgs
>   
>   let fname1 = args!!0
>       fname2 = args!!1
>
>   content1 <- parseFile fname1
>   content2 <- parseFile fname2
>   print $ match content1 content2

