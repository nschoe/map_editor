{--
----------------------------------------------------------------
-     PoKemon Breizh Map Editor .: nschoe
-     Simple map editor for PoKemon Breizh Haskell game
-     Pass the map name(w/o extension) as an argument
-     to start editing.
-     Press S to save the map, ESC to quit the editor
-     and F to toggle fullscreen.
----------------------------------------------------------------
--}

module Main where

import Control.Monad (when, forM_)
import Control.Monad.Reader
import Control.Monad.State

import Data.Array ((!), (//), listArray, Array, assocs)
import Data.Word (Word16)

import Graphics.UI.SDL as SDL

import System.Directory (copyFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, hPutStr, openFile, IOMode(..), hClose)

import Config
import Helper
import Pokemap
import Timer
import Types



-- Loads files, and init camera position...
initEnv :: FilePath -> IO (AppResource, AppData)
initEnv mapFile = do
  -- Set the window title, screen dimension and flags.
  setCaption "PoKemon Breizh Map Editor .: nschoe" []
  screen <- setVideoMode sWidth sHeight sBpp sFlags

  -- Loads the sprite sheet
  spriteSheet <- loadImage (img </> "sprite_sheet.png")

  -- Loads the map
  world <- parseMap (fmn mapFile)

  -- Start the fps timer
  fps <- start defaultTimer

  -- Creates our camera, its dimensions being the field's
  let camera = Rect 0 0 (drawWindowW * tileDim) (drawWindowH * tileDim)

  -- And set the initial tile type as null
      currentTile = 0

  -- The panel initial position
      panel = (0,0)

  -- We don't want to quit yet
      bye = False

  -- We don't want to paint yet
      painting = False

  -- Scroll is allowed
      blockScroll = False

  return (AppResource screen spriteSheet, AppData world panel fps camera currentTile bye painting blockScroll)



-- Handler events: handle clicks, camera's positionning, tile selection...
handleEvents :: String -> AppEnv ()
handleEvents mapName = do
  event <- liftIO pollEvent
  case event of
    Quit                             -> putBye True
    KeyDown (Keysym SDLK_ESCAPE _ _) -> putBye True
    NoEvent                          -> return ()
    _                                -> do
                 handleEvents' mapName event
                 handleEvents mapName



handleEvents' :: String -> Event -> AppEnv ()

-- Toggles fullscreen on F press
handleEvents' _ (KeyDown (Keysym SDLK_f _ _)) = do
  getScreen >>= liftIO . toggleFullscreen

-- Sets current tile on click or paints a new tile
--Warning: yTile and xTile are inverted because arrays are accessed maths-like
handleEvents' _ (MouseButtonDown mouseX mouseY ButtonLeft) = do
  -- Handles changing current tile
  when (inPanel (fromIntegral mouseX, fromIntegral mouseY)) $ do
    (pX, pY)           <- getPanel
    let (xTile, yTile) = (pX + ptpt (fromIntegral mouseX), pY + ptt (fromIntegral mouseY))
        newTile        = panel ! (yTile, xTile)
    putCurrentTile newTile

  -- Handles tile painting
  when (inField (mouseX', mouseY')) (putPainting True)
{-
  when (inField (mouseX', mouseY')) $ do
     camera@(Rect cx cy _ _)     <- getCamera
     world@World{wField = field} <- getWorld
     currentTile                 <- getCurrentTile
     let (xTile, yTile) = (ptt mouseX', ptt mouseY')
         (realXTile, realYTile) = (xTile + ptt cx, yTile + ptt cy)
         fieldModifier = [((realYTile, realXTile), currentTile)]
     putWorld world{wField = (field // fieldModifier)}
-}
         where (mouseX', mouseY') = (fromIntegral mouseX, fromIntegral mouseY)

handleEvents' _ (MouseButtonUp _ _ ButtonLeft) =
    putPainting False

-- Handles saving map on S press
handleEvents' mapName (KeyDown (Keysym SDLK_s _ _)) = do
  world <- getWorld
  liftIO $ storeMap world (fmn mapName)

-- Handles panel switching on mouse scrolling
handleEvents' _ (MouseButtonDown _ _ ButtonWheelDown) = do
  (pX, pY) <- getPanel
  let pX'  = pX + panelW
      pX'' = if (pX' + panelW) > iMax then (iMax+1 - panelW) else pX'
  putPanel (pX'', pY)

handleEvents' _ (MouseButtonDown _ _ ButtonWheelUp) = do
  (pX, pY) <- getPanel
  let pX'  = pX - panelW
      pX'' = if pX' < 0 then 0 else pX'
  putPanel (pX'', pY)

-- Handles creating a new TOP row
handleEvents' _ (KeyDown (Keysym SDLK_UP _ _)) = do
  world@World{wField = field, wDim=(xDim, yDim)} <- getWorld
  camera                                         <- getCamera
  let newBounds = ((0,0), (yDim, xDim-1)) -- only yDim gets expanded
      tempField = listArray newBounds (repeat 0) :: Array ((Int,Int)) Word16
      oldFieldList = assocs field
      oldFieldListOffset = map (\((i,j), c) -> ((i+1,j),c)) oldFieldList
      newField = tempField // oldFieldListOffset
  putWorld world{wField = newField, wDim=(xDim, yDim + 1)}
  putCamera camera{rectY=0} -- Sets the camera at the top (to see the newly created row)

-- Handles creating a new DOWN row
handleEvents' _ (KeyDown (Keysym SDLK_DOWN _ _)) = do
  world@World{wField = field, wDim=(xDim, yDim)} <- getWorld
  camera                                         <- getCamera
  let newBounds = ((0,0), (yDim, xDim-1)) -- only yDim gets expanded
      tempField = listArray newBounds (repeat 0) :: Array ((Int,Int)) Word16
      oldFieldList = assocs field
      newField = tempField // oldFieldList
  putWorld world{wField = newField, wDim=(xDim, yDim + 1)}
  putCamera camera{rectY=(yDim + 1) * tileDim - (drawWindowH * tileDim)} -- Sets the camera at the bottom (to see the nealy created row)

-- Handles creating a new LEFT column
handleEvents' _ (KeyDown (Keysym SDLK_LEFT _ _)) = do
  world@World{wField = field, wDim=(xDim, yDim)} <- getWorld
  camera                                         <- getCamera
  let newBounds = ((0,0), (yDim-1, xDim)) -- only xDim gets expanded
      tempField = listArray newBounds (repeat 0) :: Array ((Int,Int)) Word16
      oldFieldList = assocs field
      oldFieldListOffset = map (\((i,j), c) -> ((i, j+1), c)) oldFieldList
      newField = tempField // oldFieldListOffset
  putWorld world{wField = newField, wDim=(xDim + 1, yDim)}
  putCamera camera{rectX=0} -- Sets the camera at the left (to see the nealy created column)

-- Handles creating a new RIGHT column
handleEvents' _ (KeyDown (Keysym SDLK_RIGHT _ _)) = do
  world@World{wField = field, wDim=(xDim, yDim)} <- getWorld
  camera                                         <- getCamera
  let newBounds = ((0,0), (yDim-1, xDim)) -- only xDim gets expanded
      tempField = listArray newBounds (repeat 0) :: Array ((Int,Int)) Word16
      oldFieldList = assocs field
      newField = tempField // oldFieldList
  putWorld world{wField = newField, wDim=(xDim + 1, yDim)}
  putCamera camera{rectX=(xDim + 1) * tileDim - (drawWindowW * tileDim)} -- Sets the camera at the right (to see the nealy created column)

-- Handles block scrolling
handleEvents' _ (KeyDown (Keysym SDLK_SPACE _ _)) = do
  putBlockScroll True

handleEvents' _ (KeyUp (Keysym SDLK_SPACE _ _)) = do
  putBlockScroll False

-- Displays the tile type under the mouse
handleEvents' _ (KeyDown (Keysym SDLK_z _ _)) = do
  (mouseX, mouseY, _) <- liftIO $ getMouseState
  
  when (inField (fromIntegral mouseX, fromIntegral mouseY)) $ do
    world@World{ wField = field } <- getWorld
    (Rect cx cy _ _)              <- getCamera
    let (mX, mY) = (ptt ((fromIntegral mouseX) + cx), ptt ((fromIntegral mouseY) + cy))
        tileType = field ! (mY, mX)
    
    h <- liftIO $ openFile (root </> "forbidden") AppendMode
    liftIO $ hPutStr h ((show tileType) ++ " ")
    liftIO $ hClose h
  

handleEvents' _ _ = return ()



-- Logic processing
performLogic :: AppEnv ()
performLogic = do
  -- Moves the camera when needed
  scrollAllowed <- liftM not getBlockScroll
  camera@(Rect cx cy _ _) <- getCamera
  (mouseX, mouseY, _) <- liftIO getMouseState
  (width, height) <- getDim

  -- Right scrolling
  when (scrollAllowed && mouseX >= (drawWindowW * tileDim) - (2 * tileDim) && mouseX <= (drawWindowW * tileDim)) $ do
                 let cx'  = cx + tileDim
                     cx'' = if (ptt cx' + drawWindowW) > width then cx else cx'
                 putCamera camera{rectX = cx''}

  -- Left scrolling
  when (scrollAllowed && mouseX <= (2 * tileDim)) $ do
                 let cx'  = cx - tileDim
                     cx'' = if cx' < 0 then cx else cx'
                 putCamera camera{rectX = cx''}

  -- Top scrolling
  when (scrollAllowed && inField (mouseX, mouseY) && mouseY <= (2 * tileDim)) $ do
                 let cy'  = cy - tileDim
                     cy'' = if cy' < 0 then cy else cy'
                 putCamera camera{rectY = cy''}

  -- Bottom scrolling
  when (scrollAllowed && inField (mouseX, mouseY) && mouseY >= ((drawWindowH * tileDim) - (2 * tileDim))) $ do
                 let cy'  = cy + tileDim
                     cy'' = if (ptt cy' + drawWindowH) > height then cy else cy'
                 putCamera camera{rectY = cy''}

  -- Paints the tiles on the field
  painting <- getPainting
  when painting paintTile



-- Paints a tile on the field, at the mouse position
paintTile :: AppEnv ()
paintTile = do
  -- We get the mouse (X,Y) position
  (mouseX, mouseY, _)         <- liftIO getMouseState

  -- Then we paint
  camera@(Rect cx cy _ _)     <- getCamera
  world@World{wField = field} <- getWorld
  currentTile                 <- getCurrentTile
  let (mouseX', mouseY') = (fromIntegral mouseX, fromIntegral mouseY)
      (xTile, yTile) = (ptt mouseX', ptt mouseY')
      (realXTile, realYTile) = (xTile + ptt cx, yTile + ptt cy)
      fieldModifier          = [((realYTile, realXTile), currentTile)]
  putWorld world{wField = (field // fieldModifier)}
    


-- Perform rendering
rendering :: AppEnv ()
rendering = do
  -- Displays the world in the camera
  -- Gets resources
  world@World{wField = field, wDim = (width, height)}  <- getWorld
  screen <- getScreen
  camera@(Rect cx cy cw ch) <- getCamera
  spriteSheet <- getSpriteSheet

  -- Creates a list of the visible tiles
  let topLeftX = ptt cx
      topLeftY = ptt cy
      bottomRightX = ptt (cx + cw)
      bottomRightY = ptt (cy + ch)
      visibleTiles = [(i,j) | i <- [topLeftX..bottomRightX - 1], j <- [topLeftY..bottomRightY - 1]]

  
  -- Displays them
  liftIO $ forM_ visibleTiles $ \(i,j) -> do
              let (x,y) = (ttp i - cx, ttp j - cy) 
                  clip  = clips ! (field ! (j,i))
              applySurface x y spriteSheet screen (Just clip)

  -- Displays the panel
  (pX, pY) <- getPanel
  let panelVisibleTiles = [(i,j) | i <- [pX..pX+panelW-1], j <- [pY..pY+panelH-1]]
  liftIO $ forM_ panelVisibleTiles $ \(i,j) -> do
    let tile = panel ! (j, i)
        rec  = clips ! tile
    liftIO $ applySurface (pttp (i - pX)) (ttp (j - pY)) spriteSheet screen (Just rec)


  -- Last: Displays the current tile on the mouse
  currentTile         <- getCurrentTile
  (mouseX, mouseY, _) <- liftIO getMouseState
  let tileClip = clips ! currentTile
      (xTiles, yTiles)   = (ptt (fromIntegral mouseX), ptt (fromIntegral mouseY))
      (xPixels, yPixels) = (xTiles * tileDim, yTiles * tileDim)
  when (inField (xPixels, yPixels)) $ do
              liftIO $ applySurface xPixels yPixels spriteSheet screen (Just tileClip)
              return ()
  

-- Main loop
loop :: String -> AppEnv ()
loop mapName = do
  -- We get the exit request
  bye <- getBye
  
  -- Begin the loop if the user doesn't want to quit
  unless bye $ do
         -- First: we start the timer
         getFPS >>= liftIO . start >>= putFPS

         -- Second: the events handling process
         handleEvents mapName

         -- Third: We perform the logic part
         performLogic

         -- Third: We display the elements on the screen
         rendering
         screen <- getScreen
         liftIO $ SDL.flip screen

         -- Then we cap the frames and loop again
         ticks <- getFPS >>= liftIO . getTimerTicks
         when (ticks < secsPerFrame) $ do
                 liftIO . SDL.delay $ secsPerFrame - ticks

         loop mapName



main :: IO ()
main = withInit [InitEverything] $ do -- withInit calls quit for us
         -- Checks if we have our only argument
         args <- getArgs

         when (length args /= 1) $ do
           hPutStrLn stderr "please provide one argument: the name of the map (with no extension)"
           exitFailure

         -- Copy the basic map is the file doesn't exist.
         exists <- doesFileExist (fmn (head args))
         when (not exists) $ do
           copyFile (fmn "basic_map") (fmn (head args))

         -- Create our base environement
         (res, state) <- initEnv (head args)

         -- Run the loop
         evalStateT (runReaderT (loop (head args)) res) state

         -- If we got there, everything went well.
         exitSuccess