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
    Module       : Graphics.Rendering.OctoPlot.Gloss.Simulation
    Description  : The Gloss backend for OctoPlot: Simulation Utilities.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot.Gloss.Simulation
( Picture(..)
, ModelTransitionFunction(..)
, DisplayModelFunction(..)
, openWindowSimulate
) where

import qualified Graphics.Gloss as G

import Graphics.Rendering.OctoPlot.Base
import Graphics.Rendering.OctoPlot.Gloss.WinUtils

-------------------------------------------------------------------------------

-- | A picture used to draw models.
-- Heavily based on the Gloss package 'G.Picture' type.
data Picture = Blank                                 -- ^ A blank picture with
                                                     -- nothing inside.
             | Polygon Path                          -- ^ A solid polygon
                                                     -- described by the path.
             | Line Path                             -- ^ A line is drawed
                                                     -- between all path's
                                                     -- points.
             | Circle Float                          -- ^ A circle with the
                                                     -- given radius.
             | ThickCircle Float Float               -- ^ A circle with the
                                                     -- given radius and
                                                     -- thickness.
             | Arc Float Float Float                 -- ^ An arc between two
                                                     -- angles at the given
                                                     -- radius.
             | ThickArc Float Float Float Float      -- ^ An arc with the given
                                                     -- thickness and between
                                                     -- two angles at some
                                                     -- radius.
             | Text String                           -- ^ A text.
             | Color Color Picture                   -- ^ Changes the color of
                                                     -- the picture.
             | TColor (Float, Float, Float) Picture  -- ^ Changes the color of
                                                     -- the picture to some RGB.
             | Translate Float Float Picture         -- ^ Translates (moves) the
                                                     -- picture by a given
                                                     -- amount.
             | Rotate Float Picture                  -- ^ Rotates the picture
                                                     -- by a given angle.
             | Scale Float Float Picture             -- ^ Scales the picture by
                                                     -- a given amount (X and
                                                     -- Y).
             | Pictures [Picture]                    -- ^ A picture containing
                                                     -- only other pictures.
             deriving (Eq, Show)

-- | Advances the model one generation.
-- This is used in order to generate the next model to show.
type ModelTransitionFunction w = (w -> w)

-- | Converts the model to a 'Picture'.
type DisplayModelFunction w = (w -> Picture)

-- Converts a Picture to a Gloss Picture
pictureToGPic :: Picture -> G.Picture
pictureToGPic Blank = G.Blank
pictureToGPic (Polygon path) = G.Polygon (path :: G.Path)
pictureToGPic (Line path) = G.Line (path :: G.Path)
pictureToGPic (Circle radius) = G.Circle radius
pictureToGPic (ThickCircle thickness radius) = G.ThickCircle thickness radius
pictureToGPic (Arc st en rd) = G.Arc st en rd
pictureToGPic (ThickArc thickness st en rd) = G.ThickArc st en rd thickness
pictureToGPic (Text text) = G.Text text
pictureToGPic (Color cl pc) = G.Color cc (pictureToGPic pc)
                            where (r, g, b) = colorToRGB cl
                                  cc = G.makeColor r g b 1
pictureToGPic (TColor (r, g, b) pc) = G.Color cc (pictureToGPic pc)
                                    where cc = G.makeColor r g b 1
pictureToGPic (Translate x y pc) = G.Translate x y (pictureToGPic pc)
pictureToGPic (Rotate angle pc) = G.Rotate angle (pictureToGPic pc)
pictureToGPic (Scale x y pc) = G.Scale x y (pictureToGPic pc)
pictureToGPic (Pictures ls) = G.Pictures (map pictureToGPic ls)

-- Wraps a DisplayModelFunction
wrapTF :: DisplayModelFunction w -> w -> G.Picture
wrapTF fc = pictureToGPic . fc

-- | Opens a window that simulates something.
--
-- It is required to pass a 'ModelTransitionFunction' and a
-- 'DisplayModelFunction'. This can be used, for example, to simulate a
-- cellular automata.
openWindowSimulate :: String                     -- ^ The title of the window.
                   -> (Float, Float)             -- ^ Width and height of the
                                                 -- window in pixels.
                   -> Int                        -- ^ Number of iterations
                                                 -- per-second.
                   -> w                          -- ^ Initial state.
                   -> ModelTransitionFunction w  -- ^ Transition function.
                   -> DisplayModelFunction w     -- ^ Display function.
                   -> IO ()
openWindowSimulate title dims vel world wst wdf = G.simulate (G.InWindow
                                                                 title
                                                                 (dimToInt dims)
                                                                 (10, 10)
                                                             ) G.white
                                                             vel
                                                             world
                                                             (wrapTF wdf)
                                                             (\_ _ -> wst)
