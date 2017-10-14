{-
  OctoPlot Visualization Library.
  Copyright (C) 2017  Alejandro Linarez Rangel

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-|
    Module       : Graphics.Rendering.OctoPlot.Gloss.WinUtils
    Description  : The Gloss backend for OctoPlot: Window utilities.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot.Gloss.WinUtils
( DisplayCallback(..)
, dimToInt
, convert
, openWindow
, displayNothing
) where

import Graphics.Gloss ( Picture (Blank)
                      , Display (InWindow)
                      , display
                      , simulate
                      , white
                      )

import Graphics.Rendering.OctoPlot.Base

-------------------------------------------------------------------------------

-- | A callback used to display things.
-- The first argument is the window dimensions in pixels, in the form
-- @(width, height)@. The returned 'Picture' will be drawed.
type DisplayCallback = ((Float, Float) -> Picture)

-- | Convert OpenGL-like coordinates to Gloss ones.
-- OpenGL manages the graphics in the range @[-1, 1]@, but Gloss uses pixels
-- for the coordinates.
convert :: Point           -- ^ Initial coordinates in the rando @[-1, 1]@.
        -> (Float, Float)  -- ^ Width and height of the window, in pixels.
        -> (Float, Float)  -- ^ Converted coordinates.
convert (x, y) (w, h) = (x * w, y * h)

-- | Converts Float coordinates to Int ones.
-- It's just a cast.
dimToInt :: (Float, Float) -> (Int, Int)
dimToInt (a, b) = ( (fromIntegral $ toInteger $ round a) :: Int
                  , (fromIntegral $ toInteger $ round b) :: Int)

-- | Opens a window and draws something in it.
openWindow :: String           -- ^ The title of the window.
           -> (Float, Float)   -- ^ Width and height of the window in pixels.
           -> DisplayCallback  -- ^ Callback used to draw things.
           -> IO ()
openWindow title dims cll = display (InWindow
                                              title
                                              (dimToInt dims)
                                              (10, 10)
                                    ) white (cll dims)

-- | A simple display callback that draws nothing.
displayNothing :: (PointMap, Option) -> DisplayCallback
displayNothing _ _ = Blank
