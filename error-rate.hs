-- Use Lazy IO instead of default IO.
import Prelude hiding (readFile, writeFile, lines, unlines)
import Data.Text.Lazy    (Text, lines)
import Data.Text.Lazy.IO (readFile)

-- Parser for reading CSV files
import Data.Attoparsec.Text.Lazy (decimal, parse, maybeResult)

-- Useful generic functions
import Data.Maybe (fromJust)

-- Data structures for storing data
import Data.Word (Word8)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec (sum, zipWith, fromList, length)

import System.Environment (getArgs)

vectorize :: [Text] -> Vector Word8
vectorize = Vec.fromList . map readLine 
     where
           readLine :: Text -> Word8
           readLine = fromJust . maybeResult . parse decimal

correct :: Vector Word8 -> Vector Word8 -> Double
correct ref res = Vec.sum correct / size
    where correct = Vec.zipWith diff ref res
          diff x y = if x == y then 1 else 0
          size = fromIntegral . Vec.length $ ref

main :: IO ()
main = let 
           parseFile :: String -> IO [Text]
           parseFile = fmap (lines) . readFile
       in
       do
       -- Reference file
       args <- getArgs 
       let refName     = args !! 0
           resultName  = args !! 1

       refInput <- parseFile refName
       resultInput <- parseFile resultName

       let refData    = vectorize refInput
           resultData = vectorize resultInput

           c = correct refData resultData

       print c


