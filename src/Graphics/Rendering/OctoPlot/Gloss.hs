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
    Module       : Graphics.Rendering.OctoPlot.Gloss
    Description  : The Gloss backend for OctoPlot.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot.Gloss
(
  -- * Continuous plotting
  plot
, plotN
  -- * Scatter plotting
, scatter
, scatterN
  -- * Scatter plotting and Continuous functions
, scatterAndPlot
  -- * Simulation.
, simulate
  -- * Functions and types reexported
  -- From Graphics.Rendering.OctoPlot.Gloss.Simulation:
, Picture(..)
, ModelTransitionFunction(..)
, DisplayModelFunction(..)
) where

import qualified Graphics.Gloss as G

import Graphics.Rendering.OctoPlot.Base
import Graphics.Rendering.OctoPlot.Gloss.WinUtils
import Graphics.Rendering.OctoPlot.Gloss.LinePlot2D
import Graphics.Rendering.OctoPlot.Gloss.Simulation

-------------------------------------------------------------------------------

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
plotN values gopts = openWindow "Plot 2D" (400, 400)
                         (displayLineListSimple values gopts displayLine)

-- | Shows points from a dataset.
-- If you are searching for a function to plot continuous functions, see
-- 'plot' and 'plotN'.
--
-- This is equivalent to @scatterN [(x, y, opts)] opts@.
--
-- For example:
--
-- > import Graphics.Rendering.OctoPlot
-- >
-- > x :: Linspace Float
-- > x = linspace (-2 * pi) 20.0 (2 * pi)
-- > y :: Linspace Float
-- > y = (\x -> abs (sin x) * x) <$> x
-- >
-- > opts :: Option
-- > opts = mkDefaultOption { legendOption = "Some figure!" }
-- >
-- > main = scatter x y opts
--
scatter :: (Plotable a)
        => Linspace a  -- ^ X axis to scatter.
        -> Linspace a  -- ^ Y axis to scatter.
        -> Option      -- ^ Global options.
        -> IO ()
scatter x y opts = scatterN [(x, y, opts)] opts

-- | Shows multiples points from a dataset.
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
-- > x = linspace (-2 * pi) 10.0 0.0
-- > y :: Linspace Float
-- > y = linspace 0.0 10.0 (2 * pi)
-- >
-- > w :: Linspace Float -> Linspace Float
-- > w a = (\b -> abs (sin b) * b) <$> a
-- >
-- > opts :: Option
-- > opts = mkDefaultOption { legendOption = "Some figure!" }
-- >
-- > xopt :: Option
-- > xopt = mkDefaultOption { penColorOption = Red }
-- > yopt :: Option
-- > yopt = mkDefaultOption { penColorOption = Magenta }
-- >
-- > main = scatterN [(x, w x, xopt), (y, w y, yopt)] opts
--
scatterN :: (Plotable a)
         => [(Linspace a, Linspace a, Option)]  -- ^ Datasets to scatter.
         -> Option                              -- ^ Global options.
         -> IO ()
scatterN values gopts = openWindow "Scatter 2D" (400, 400)
                            (displayLineListSimple values gopts displayPoints)

-- | Scatters and plots two set of datasets in the same window.
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
-- > x = linspace (-2 * pi) 10.0 0.0
-- > y :: Linspace Float
-- > y = linspace 0.0 10.0 (2 * pi)
-- > z :: Linspace Float
-- > z = linspace (-2 * pi) 200.0 (2 * pi)
-- >
-- > w :: Linspace Float -> Linspace Float
-- > w a = (\b -> abs (sin b) * b) <$> a
-- >
-- > opts :: Option
-- > opts = mkDefaultOption { legendOption = "Some figure!"
-- >                        , gridModeOption = UseBiggest }
-- >
-- > xopt :: Option
-- > xopt = mkDefaultOption { penColorOption = Red }
-- > yopt :: Option
-- > yopt = mkDefaultOption { penColorOption = Magenta }
-- > zopt :: Option
-- > zopt = mkDefaultOption { penColorOption = Blue }
-- >
-- > rx = w x
-- > ry = w y
-- > rz = w z
-- >
-- > main = scatterAndPlot [(z, rz, zopt)] [(x, rx, xopt), (y, ry, yopt)] opts
--
scatterAndPlot :: (Plotable a)
               => [(Linspace a, Linspace a, Option)]  -- ^ Datasets to plot.
               -> [(Linspace a, Linspace a, Option)]  -- ^ Datasets to scatter.
               -> Option                              -- ^ Global options.
               -> IO ()
scatterAndPlot toPlot toScatter gopts = openWindow
                                            "Scatter and Plot 2D"
                                            (400, 400)
                                            (displayLineList
                                                [toPlot, toScatter]
                                                gopts
                                                [displayLine, displayPoints])

-- | Simulates a model and display it as an animation.
--
-- Based on the 'openWindowSimulate' function, this will display your model
-- at some time steps.
--
-- You decide how to display your model ('DisplayModelFunction') and how to
-- advance one iteration ('ModelTransitionFunction').
--
-- For example:
--
-- > import Graphics.Rendering.OctoPlot
-- >
-- > data Model = Model { counter :: !Int }
-- >
-- > transitionF :: Model -> Model
-- > transitionF m = Model { counter = counter m + 1 }
-- >
-- > displayF :: Model -> Picture
-- > displayF Model { counter = c } = Text $ show c
-- >
-- > main = simulate 1
-- >                 Model { counter = 1 }
-- >                 transitionF
-- >                 displayF
--
simulate :: Int                        -- ^ Speed of the simulation (steps per
                                       -- second).
         -> w                          -- ^ Initial model.
         -> ModelTransitionFunction w  -- ^ Transition function.
         -> DisplayModelFunction w     -- ^ Display function.
         -> IO ()
simulate = openWindowSimulate "Simulation" (400, 400)
