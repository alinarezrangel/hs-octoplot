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
    Module       : Graphics.Rendering.OctoPlot.EasyPlot
    Description  : The EasyPlot backend for OctoPlot.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot.EasyPlot
(
  -- * Continuous plotting
  plot
, plotN
) where

import qualified Graphics.EasyPlot as G

import Control.Monad (void)

import Graphics.Rendering.OctoPlot.Base

-------------------------------------------------------------------------------

-- Converts a 'Color' to a 'G.Color'
colorToGColor :: Color -> G.Color
colorToGColor Red = G.Red
colorToGColor Green = G.Green
colorToGColor Blue = G.Blue
colorToGColor Magenta = G.Magenta
colorToGColor Cyan = G.Cyan
colorToGColor Yellow = G.Yellow
colorToGColor White = G.White
colorToGColor Black = G.Black

lstyleToGStyle :: LineStyle -> G.Style
lstyleToGStyle LineStyle = G.Lines
lstyleToGStyle DotsStyle = G.Dots
lstyleToGStyle DashStyle = G.Points
lstyleToGStyle DashDotStyle = G.Lines

convertOptionToGOption :: Option -> Option -> [G.Option]
convertOptionToGOption opt gop = [ G.Title newttl
                                 , G.Color $ colorToGColor
                                                 $ penColorOption opt
                                 , G.Style $ lstyleToGStyle
                                                 $ lineStyleOption opt
                                 ]
                               where defttl = legendOption mkDefaultOption
                                     ttl = legendOption opt
                                     gttl = legendOption gop
                                     newttl = if ttl == defttl
                                                  then gttl
                                                  else ttl

mapplot2D :: (Plotable a)
          => (Linspace a, Linspace a, Option)
          -> Option
          -> G.Graph2D a a
mapplot2D (x, y, opt) gopt = G.Data2D (convertOptionToGOption opt gopt)
                                      []
                                      (zip x y)

-- | Plots a dataset assuming that it represents a continuous function.
--
-- For example:
--
-- > import Graphics.Rendering.OctoPlot
-- >
-- > x :: Linspace Float
-- > x = linspace 0.0 100.0 pi
-- > y :: Linspace Float
-- > y = sin <$> x  -- Remember: linspace are just lists of scalars
-- >
-- > opts :: Option
-- > opts = mkDefaultOption { xLabelOption = "0-pi"
-- >                        , yLabelOption = "sin x" }
-- >
-- > main = plot x y opts
--
-- As you can see, this function is equivalent to @plotN [(x, y, opts)] opts@.
--
plot :: (Plotable a)
     => Linspace a  -- ^ The X (horizontal) axis of the plot.
     -> Linspace a  -- ^ The Y (vertical) axis of the plot.
     -> Option      -- ^ The options, used both as global and local.
     -> IO ()
plot x y opts = plotN [(x, y, opts)] opts

-- | Plots multiples datasets.
--
-- Each triplet will have the form @(x, y, opts)@ where the 'Option's contained
-- in the triplets will be applied __only to that plot__. Not all options are
-- supported in local or global options:
--
--     * The /local options/ are the options contained in the triplet, you
--       can use it to change the pen color, marker type, marker size and
--       line style.
--     * The /global options/ are the options passed as it's own argument and
--       them will modify all plots, the supported global options are used to
--       change the legend, x-label, y-label, grid style and grid mode.
--
-- For example:
--
-- > import Graphics.Rendering.OctoPlot
-- >
-- > x :: Linspace Float
-- > x = linspace (-2 * pi) 200.0 (2 * pi)
-- > y :: Linspace Float
-- > y = sin <$> x
-- > z :: Linspace Float
-- > z = cos <$> x
-- >
-- > opts :: Option
-- > opts = mkDefaultOption { legendOption = "Some figure!" }
-- >
-- > yopt :: Option
-- > yopt = mkDefaultOption { penColorOption = Red }
-- >
-- > zopt :: Option
-- > zopt = mkDefaultOption { penColorOption = Magenta }
-- >
-- > main = plotN [(x, y, yopt), (x, z, zopt)] opts
--
plotN :: (Plotable a)
      => [(Linspace a, Linspace a, Option)]  -- ^ Datasets to plots.
      -> Option                              -- ^ Global options.
      -> IO ()
plotN values gopts = void $ G.plot G.X11 ls
                   where ls = map (`mapplot2D` gopts) values
