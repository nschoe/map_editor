{-
----------------------------------------------------------------
-     This module regroups constants, and config
-     features needed by the project.
----------------------------------------------------------------
-}

module Config (
                sWidth
              , sHeight
              , sBpp
              , sFlags
              , framesPerSecond
              , secsPerFrame
              , tileDim
              , drawWindowW
              , drawWindowH
              , panelW
              , panelH
              , separatorW
              , separatorH
              , clips
              , grid
              , panel
              , nbOfTiles
              , iMax
              , jMax
              ) where

import Data.Array
import Data.Word (Word16,Word32)

import Graphics.UI.SDL



sWidth, sHeight, sBpp :: Int
sWidth   = 1056
sHeight  = 768
sBpp     = 32

secsPerFrame, framesPerSecond :: Word32
framesPerSecond = 24
secsPerFrame    = 1000 `div` framesPerSecond

sFlags :: [SurfaceFlag]
sFlags = [SWSurface]

-- Defined in pixels
tileDim :: Int
tileDim = 48

-- Defined in tiles
drawWindowW, drawWindowH, panelW, panelH, separatorW, separatorH, iMax, jMax, nbOfTiles :: Int
drawWindowW = 16
drawWindowH = 16
panelW      = 5
panelH      = 16
separatorW  = 1
separatorH  = 16
iMax        = 99
jMax        = 15
nbOfTiles   = 1599

-- Huge array used to clip sprites from the sprite sheet
clips :: Array Word16 Rect --Alist
clips = listArray (0, (fromIntegral nbOfTiles)) $ map grid [(j,i) | i <- [0..jMax], j <- [0..iMax]]
{-clips = listArray (0, n) [grid (j,i) | i <- [0..iMax], j <- [0..jMax]]
    where iMax' = fromIntegral iMax
          jMax' = fromIntegral jMax
          n     = fromIntegral $ (iMax+1) * (jMax+1) - 1
-}
{-clips = listArray (0, nbOfTiles) [
                          grid (0,0) -- 0 : Null
                        , grid (1,0) -- 1 : Grass
                        , grid (2,0) -- 2 : Rough
                        , grid (3,0) -- 3 : Right Jump
                        , grid (4,0) -- 4 : Left Jump
                        , grid (5,0) -- 5 : Bottom Right Jump
                        , grid (6,0) -- 6 : Top Right Jump
                        , grid (7,0) -- 7 : Top Left Jump
                        , grid (8,0) -- 
                         ]-}



-- Converts a (x,y) grid position to a SDL Rect (used for populating 
--  the sprite sheet).
--  Warning: (x,y) is given SDL-like, not matrix-like, BUT starting a zero
grid :: (Int, Int) -> Rect
grid (x,y) = Rect rX rY rW rH
    where rW = tileDim
          rH = tileDim
          rX = x * tileDim
          rY = y * tileDim



-- The whole panel, containing all sprites
-- Warning: arrays are accessed maths-like
--          panel is filled LINE-BY-LINE
panel :: Array (Int, Int) Word16
panel = listArray ((0,0), (jMax, iMax)) $ ([0..fromIntegral (nbOfTiles)] ++ [0,0..])