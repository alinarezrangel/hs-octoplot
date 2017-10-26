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
    Module       : Graphics.Rendering.OctoPlot.Base
    Description  : Base functions and utilities.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot.Base
( Linspace(..)
, MarkerType(..)
, LineStyle(..)
, Color(..)
, Option(..)
, Plotable(..)
, GridStyle(..)
, GridMode(..)
, AspectRatio(..)
, Point(..)
, Path(..)
, PointMap(..)
, mkDefaultOption
, colorToRGB
, linspace
, getLinspaceAR
, linspaceToPointMap
, scale
) where

import Data.List (take, map)

-------------------------------------------------------------------------------

-- | The type of the visual marker.
data MarkerType = DotMark        -- ^ Uses dots.
                | ExMark         -- ^ Uses @X@'s.
                | CrosshairMark  -- ^ Uses crosshairs (@+@)
                | CircleMark     -- ^ Uses circles.
                | StarMark       -- ^ Uses stars.
                | SquareMark     -- ^ Uses squares (or cubes).
                | DiamondMark    -- ^ Uses diamonds.
                | TriangleMark   -- ^ Uses triangles (or pyramids).
                deriving (Enum, Show, Eq, Read)

-- | The style of the line used when plotting.
data LineStyle = LineStyle     -- ^ A solid line.
               | DotsStyle     -- ^ A dotted line.
               | DashStyle     -- ^ A dashed line.
               | DashDotStyle  -- ^ A line alternating dots and dashes.
               deriving (Enum, Show, Eq, Read)

-- | Colors availables when plotting.
data Color = Black    -- ^ Black.
           | Red      -- ^ Red.
           | Green    -- ^ Green.
           | Blue     -- ^ Blue.
           | Cyan     -- ^ Cyan.
           | Yellow   -- ^ Yellow.
           | Magenta  -- ^ Magenta.
           | White    -- ^ White.
           deriving (Enum, Show, Eq, Read)

-- | Grid used when plotting.
data GridStyle = AxeStyle    -- ^ Draw lines close to the axes.
               | LinesStyle  -- ^ Draw a grid madded by solid lines.
               deriving (Enum, Show, Eq, Read)

-- | The grid mode used when plotting.
data GridMode = UseFirst    -- ^ Use the aspect ratio of the first linspace.
              | UseBiggest  -- ^ Use the biggest aspect ratio of all linspaces.
              | UseSmallest -- ^ Use the smallest aspect ratio of all linspaces.
              deriving (Enum, Show, Eq, Read)

-- | Options available when plotting.
-- These options modify the visual aspect of the plotting functions.
data Option = Option
            { penColorOption :: Color         -- ^ The drawing color.
            , markerTypeOption :: MarkerType  -- ^ The marker type.
            , markerSizeOption :: Float       -- ^ The size of the marker.
            , lineStyleOption :: LineStyle    -- ^ The line style.
            , legendOption :: String          -- ^ The plot legend.
            , xLabelOption :: String          -- ^ The X-axis legend.
            , yLabelOption :: String          -- ^ The Y-axis legend.
            , zLabelOption :: String          -- ^ The Z-axis legend.
            , gridStyleOption :: GridStyle    -- ^ The grid style.
            , gridModeOption :: GridMode      -- ^ The grid mode.
            } deriving (Show, Eq, Read)

-- | An aspect ratio.
-- Most used by linspaces when plotting.
data AspectRatio = Float :/ Float deriving (Show, Eq, Read)

-- | A point in the 2D space.
type Point = (Float, Float)

-- | A list of points is a path.
type Path = [Point]

-- | It's a transformation of linspaces.
-- Using the 'linspaceToPointMap' function, you can convert two linspaces to
-- a 'PointMap'. The point map contains the points ready to be plotted (they
-- are in the range [-1, 1], so you need to scale them) and an 'AspectRatio'
-- which determines the relation width/height of the resulting pointmap.
data PointMap = PointMap { points :: [Point]        -- ^ Points to plot.
                         , relation :: AspectRatio  -- ^ Aspect Ratio.
                         } deriving (Show, Eq, Read)

-- | A linspace.
--
-- A linspace is basicly a list of a 'Plotable' type.
--
-- Linspaces can be created with the function 'linspace'.
type Linspace a = [a]

-- | Creates the default set of options.
-- You can customize the plots by modifying the returned object, for example:
--
-- > mkDefaultOption { markerSizeOption = 15.0 } -- increases the marker size
--
-- Will show bigger marks when scattering. For a description of each field,
-- see the 'Option' type.
mkDefaultOption :: Option
mkDefaultOption = Option { penColorOption = Blue
                         , markerTypeOption = DotMark
                         , markerSizeOption = 5.0
                         , lineStyleOption = LineStyle
                         , legendOption = "Figure"
                         , xLabelOption = "X"
                         , yLabelOption = "Y"
                         , zLabelOption = "Z"
                         , gridStyleOption = AxeStyle
                         , gridModeOption = UseSmallest
                         }

-- | Converts a color to a RGB.
-- The returned triplet will have components between 0 and 1 representing the
-- red, green and blue components.
colorToRGB :: Color -> (Float, Float, Float)
colorToRGB Black   = (0.0, 0.0, 0.0)
colorToRGB Red     = (1.0, 0.0, 0.0)
colorToRGB Green   = (0.0, 1.0, 0.0)
colorToRGB Blue    = (0.0, 0.0, 1.0)
colorToRGB Cyan    = (0.0, 1.0, 1.0)
colorToRGB Yellow  = (1.0, 1.0, 0.0)
colorToRGB Magenta = (1.0, 0.0, 1.0)
colorToRGB White   = (1.0, 1.0, 1.0)

-- | Creates a linspace.
--
-- Use it like @linspace a b c@ where:
--
--     * @a@ is the starting value of the linspace
--       (@minimum (linspace a b c) == a@).
--     * @b@ is the increment constant value
--       (@((linspace a b c) !! n) - ((linspace a b c) !! (n - 1)) = b@).
--     * @c@ is the ending value of the linspace
--       (@maximum (linspace a b c) == c@).
linspace :: (Plotable a)
            => a           -- ^ The starting value.
            -> a           -- ^ The constant increment.
            -> a           -- ^ The ending value.
            -> Linspace a  -- ^ The created linspace.
linspace start size end = take (isize + 1) $ lambda <$> [0,1..]
                        where isize = round sizf :: Int -- size as int
                              incr = (enf - starf) / sizf
                              starf = toFloat start
                              sizf = toFloat size
                              enf = toFloat end
                              lambda x = fromFloat $ (x * incr) + starf

-- | Gets the aspect ratio of a linspace.
-- This determines the relation width-height of two float linspaces.
getLinspaceAR :: [Float]
              -> [Float]
              -> AspectRatio
getLinspaceAR x y = ragX :/ ragY
                  where minX = abs $ minimum x
                        minY = abs $ minimum y
                        maxX = abs $ maximum x
                        maxY = abs $ maximum y
                        rsX = max minX maxX
                        rsY = max minY maxY
                        ragX = 1.0 / rsX
                        ragY = 1.0 / rsY

-- | Converts two linspaces to a 'PointMap'.
-- Use this to plot two linspaces.
linspaceToPointMap :: (Plotable a)
                   => Linspace a
                   -> Linspace a
                   -> PointMap
linspaceToPointMap x y = PointMap { points = zip newX newY
                                  , relation = rt
                                  }
                       where rt@(ragX :/ ragY) = getLinspaceAR fx fy
                             newX = map (* ragX) fx
                             newY = map (* ragY) fy
                             fx = toFloat <$> x
                             fy = toFloat <$> y

-- | Scales a 'PointMap' to a new 'AspectRatio'.
scale :: PointMap     -- ^ 'PointMap' to scale.
      -> AspectRatio  -- ^ Scaling factor.
      -> PointMap     -- ^ Scaled 'PointMap'.
scale pm rt@(nwX :/ nwY) = PointMap { points = nwm
                                    , relation = rt
                                    }
                         where pts = points pm
                               scX :/ scY = relation pm
                               nwm = map (\(x, y) ->
                                             ((x * scX) / nwX, (y * scY) / nwY))
                                         pts

-- | Any real number that can be used in plotting functions.
-- Basicly, any real number that can be converted to a float.
class (RealFloat a, Enum a, Show a) => Plotable a where
    toFloat :: a -> Float
    fromFloat :: Float -> a

instance Plotable Float where
    toFloat = id
    fromFloat = id

instance Plotable Double where
    toFloat x = realToFrac x :: Float
    fromFloat x = realToFrac x :: Double

instance Ord AspectRatio where
    (a :/ b) `compare` (c :/ d) = (a * d) `compare` (b * c)

--
