{--
----------------------------------------------------------------
-     This module simply defines all the custom types
-     used in the map editor.
----------------------------------------------------------------
--}

module Types (
               Pixels
             , Camera
             , Alist
             , AppResource(..)
             , AppData(..)
             , AppState
             , AppEnv
             , Panel
             ) where

import Control.Monad.Reader
import Control.Monad.State

import Data.Array
import Data.Word (Word16)

import Graphics.UI.SDL (Rect, Surface)

import Pokemap
import Timer

{-
*****************************************************************
*                      Type synonyms
*****************************************************************
-}

-- Used to clarify functions signature when they need the dimensions in tiles
type Pixels = Int

type Camera = Rect

type Alist  = Array Int Rect

type AppState = StateT AppData IO

type AppEnv   = ReaderT AppResource AppState

-- Panel's top left corner coordinates (in tiles, relative to sprite sheet)
type Panel = (Int, Int)

{-
*****************************************************************
*                      End of Type synonyms
*****************************************************************
-}


{-
*****************************************************************
*                      Custom Data Types
*****************************************************************
-}

-- Read-only material
data AppResource = AppResource {
      resScreen      :: Surface -- the screen to blit on
    , resSpriteSheet :: Surface -- the sprites
    } deriving (Show)

-- State of the application
data AppData = AppData {
      appWorld        :: World     -- the map
    , appPanel        :: Panel     -- top left corner of the panel (in tiles)
    , appFps          :: Timer     -- to cap frame rate
    , appCamera       :: Camera    -- our field of vision
    , appCurrentTile  :: Word16    -- the tile we are currently painting with
    , appBye          :: Bool      -- set to true for quitting
    , appPainting     :: Bool      -- set for continuuous painting
    , appBlockScroll  :: Bool
    } deriving (Show)

{-
*****************************************************************
*                      End of Custom Data Type
*****************************************************************
-}