module Window (windowInit, createWarningWindow) where

import Data.Bits
import Graphics.X11.Xlib

windowInit :: IO (Display, ScreenNumber, Screen)
windowInit = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr  = defaultScreenOfDisplay dpy
  return $ (dpy, dflt, scr)

mkUnmanagedWindow :: Display -> Screen -> Window -> Position ->
                     Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
 let visual   = defaultVisualOfScreen scr
     attrmask = cWOverrideRedirect .|. cWBorderPixel .|. cWBackPixel
 win <- allocaSetWindowAttributes $ \attributes -> do
     set_override_redirect attributes False
     set_background_pixel attributes $ whitePixel dpy (defaultScreen dpy)
     set_border_pixel attributes $ blackPixel dpy (defaultScreen dpy)
     createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr)
      inputOutput visual attrmask attributes
 return win

drawText :: Display -> Window -> IO ()
drawText dpy win = do
 drawInWin dpy win "Watch out! Battery about to die!"
 sync dpy False
 
drawInWin :: Display -> Window -> String ->IO ()
drawInWin dpy win str = do
 bgcolor <- initColor dpy "white"
 fgcolor <- initColor dpy "black"
 gc <- createGC dpy win
 fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
 p <- createPixmap dpy win 400 100 (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
 setForeground dpy gc bgcolor
 fillRectangle dpy p gc 0 0 400 100
 setForeground dpy gc fgcolor
 fillRectangle dpy p gc 4 4 396 96
 printString dpy p gc fontStruc str
 copyArea dpy p win gc 0 0 400 100 0 0
 freeGC dpy gc
 freeFont dpy fontStruc
 freePixmap dpy p
 
initColor :: Display -> String -> IO Pixel
initColor dpy color = do
 let colormap = defaultColormap dpy (defaultScreen dpy)
 (apros,real) <- allocNamedColor dpy colormap color
 return $ color_pixel apros

printString :: Display -> Drawable -> GC -> FontStruct -> String -> IO ()
printString dpy d gc fontst str = do
 let strLen = textWidth fontst str
     (_,asc,_,_) = textExtents fontst str
     valign = (100 + fromIntegral asc) `div` 2
     remWidth = 400 - strLen
     offset = remWidth `div` 2
 fgcolor <- initColor dpy "white"
 bgcolor <- initColor dpy "black"
 setForeground dpy gc fgcolor
 setBackground dpy gc bgcolor
 drawImageString dpy d gc offset valign str
 
createWarningWindow :: Display -> ScreenNumber -> Screen -> IO ()
createWarningWindow dpy dflt scr = do
  rootw <- rootWindow dpy dflt
  win <- mkUnmanagedWindow dpy scr rootw 0 0 400 100
  setTextProperty dpy win "Battery Warning!" wM_NAME
  mapWindow dpy win
  drawText dpy win
