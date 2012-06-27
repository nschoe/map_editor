{-
----------------------------------------------------------------
-     This module is intended to parse a .pokemap file,
-     read it and save it
----------------------------------------------------------------
-}

module Pokemap (
                 World(..)
               , parseMap
               , storeMap
               ) where


import Control.Monad (liftM)

import Data.Array
import Data.List (groupBy)
import Data.Word (Word16)

import System.FilePath (FilePath, (</>))





-- World representation : stored as Word16 for memory efficiency
data World = World {
      wField    :: Array (Int, Int) Word16
    , wDim      :: (Int, Int) -- (x, y) dimension (in tiles)
    } deriving (Show, Eq)



-- Parses a map from a filename
parseMap :: FilePath -> IO World
parseMap mapName = do
  -- Reads and lines the whole file
  rawMap <- liftM lines $ readFile mapName

  -- Gets the tiles, but still in String form
  let stringedMap = map words rawMap

  -- Converts each String to a Word16
      convertedAndJoined = map (map read) stringedMap

  -- Get the dimension (assuming the map is a rectangle)
      fieldW = length (head convertedAndJoined)
      fieldH = length convertedAndJoined

  -- Populates the array
  -- WARNING: arrays are accessed maths-like ((i,j) = (line, column)) /= SDL
      field = listArray ((0,0), (fieldH - 1, fieldW - 1)) (concat convertedAndJoined)

  -- And returns the world
  return $ World field (fieldW, fieldH)




-- Stores a map to a file
storeMap :: World -> FilePath -> IO ()
storeMap world mapName = do
  -- Get dimensions
  let (fieldW, fieldH) = wDim world

  -- Get field
      arrayField = wField world

  -- Turns into a list
      listField = elems arrayField
--      listField = map snd $ assocs arrayField
      
  -- Separate into lists of correct length (map width)
      dimensionnedFieldList = splitEvery fieldW listField

  -- Serialize the Word16s to be ready to be written on file
      serializedList = map serializeWord16 dimensionnedFieldList

  -- Make it one big line to be written
      oneString = unlines serializedList

  -- Write to file
  writeFile mapName oneString



-- Same at splitAt, but does it repetedly
splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = let (l, xs') = splitAt n xs
                  in l : case xs' of
                           []     -> []
                           xs''   -> splitEvery n xs''



-- Function to render a list of Word16 to a ready-to-be-stored String
serializeWord16 :: [Word16] -> String
serializeWord16 [] = ""
serializeWord16 (x:[]) = show x
serializeWord16 (x:xs) = show x ++ " " ++ serializeWord16 xs