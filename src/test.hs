import Graphics.UI.SDL as SDL

import Helper
import Config

main = withInit [InitEverything] $ do
         screen <- setVideoMode sWidth sHeight sBpp sFlags
         img <- loadImage "../img/sprite_sheet.png"
         applySurface 48 0 img screen (Just $ Rect 0 0 48 48)
         SDL.flip screen
         delay 2000