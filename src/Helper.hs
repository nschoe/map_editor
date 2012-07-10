{-
----------------------------------------------------------------
-     This module provides helper functions to be
-     used accross the project, mostly to save
-     key typing, do conversions, etc.
----------------------------------------------------------------
-}

{-# LANGUAGE FlexibleContexts #-}

module Helper (
                loadImage
              , applySurface
              , root
              , img
              , sound
              , mapDir
              , ttp
              , ptt
              , ptpt
              , pttp
              , fmn
              , inField
              , inPanel
              , getScreen
              , getSpriteSheet
              , getWorld
              , putWorld
              , getDim
              , getFPS
              , putFPS
              , getCamera
              , putCamera
              , getCurrentTile
              , putCurrentTile
              , getPanel
              , putPanel
              , getBye
              , putBye
              , getPainting
              , putPainting
              , getBlockScroll
              , putBlockScroll
              ) where

import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.Reader

import Data.Word (Word16)

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

import System.FilePath (FilePath, (</>))

import Config (tileDim, drawWindowW, drawWindowH, panelW, panelH, separatorW)
import Pokemap (World(..))
import Timer (Timer(..))
import Types



-- Optimize-loads an image (any format), keeping alpha channel
loadImage :: FilePath  -> IO Surface
loadImage filename = load filename >>= displayFormatAlpha



-- Blits a surface, allowing for clipping
applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
    where offset = Just Rect {rectX=x, rectY=y, rectW=0, rectH=0}


          
-- Returns the application's root directory
root :: FilePath
root = ".."



-- Returns the application's image directory
img :: FilePath
img = root </> "img"



-- Returns the application's sound directory
sound :: FilePath
sound = root </> "snd"



-- Returns the application's map directory
mapDir :: FilePath
mapDir = root </> "maps"



-- Converts dimension from tiles to pixels
-- TileToPixels
ttp :: Int -> Pixels
ttp n = n * tileDim



-- Converts dimension from pixels to tiles
-- PixelsToTile
ptt :: Pixels -> Int
ptt n = n `div` tileDim



-- Converts dimension from pixels to panel tiles
-- PixelsToPanelTile
ptpt :: Pixels -> Int
ptpt n = ptt n - drawWindowW - separatorW



-- Converts dimension from panel tiles to pixels
-- PanelTileToPixels
pttp :: Int -> Pixels
pttp n = ttp n + drawWindowW * tileDim + separatorW * tileDim



-- Creates full map name (file path) from a map name
-- FullMapName
fmn :: String -> FilePath
fmn mapName = mapDir </> mapName ++ ".pokemap"



-- Shortcut function to hover the field
inField :: (Int, Int) -> Bool
inField (x,y) = (x < drawWindowW * tileDim)



-- Shortcut function to hover the panel
inPanel :: (Int, Int) -> Bool
inPanel (x,y) = (x >= drawWindowW * tileDim + separatorW * tileDim)



-- Accessor functions
-- AppResource (MonadReader)
getScreen :: MonadReader AppResource m => m Surface
getScreen = liftM resScreen ask

getSpriteSheet :: MonadReader AppResource m => m Surface
getSpriteSheet = liftM resSpriteSheet ask

-- AppData (MonadState)
getWorld :: MonadState AppData m => m World
getWorld = liftM appWorld get

putWorld :: MonadState AppData m => World -> m ()
putWorld w = modify $ \s -> s { appWorld = w }

getDim :: MonadState AppData m => m (Int, Int)
getDim = liftM (wDim . appWorld) get

getFPS :: MonadState AppData m => m Timer
getFPS = liftM appFps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { appFps = t }

getCamera :: MonadState AppData m => m Camera
getCamera = liftM appCamera get

putCamera :: MonadState AppData m => Camera -> m ()
putCamera c = modify $ \s -> s { appCamera = c }

getCurrentTile :: MonadState AppData m => m Word16
getCurrentTile = liftM appCurrentTile get 

putCurrentTile :: MonadState AppData m => Word16 -> m ()
putCurrentTile t = modify $ \s -> s { appCurrentTile = t }

getPanel :: MonadState AppData m => m Panel
getPanel = liftM appPanel get

putPanel :: MonadState AppData m => Panel -> m ()
putPanel p = modify $ \s -> s { appPanel = p }

getBye :: MonadState AppData m => m Bool
getBye = liftM appBye get

putBye :: MonadState AppData m => Bool -> m ()
putBye b = modify $ \s -> s { appBye = b }

getPainting :: MonadState AppData m => m Bool
getPainting = liftM appPainting get

putPainting :: MonadState AppData m => Bool -> m ()
putPainting b = modify $ \s -> s { appPainting = b }

getBlockScroll :: MonadState AppData m => m Bool
getBlockScroll = liftM appBlockScroll get

putBlockScroll :: MonadState AppData m => Bool -> m ()
putBlockScroll b = modify $ \s -> s { appBlockScroll = b }