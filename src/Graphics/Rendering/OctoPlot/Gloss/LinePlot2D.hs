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
    Module       : Graphics.Rendering.OctoPlot.Gloss.LinePlot2D
    Description  : The Gloss backend for OctoPlot: 2D Function Plotting.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot.Gloss.LinePlot2D
( displayLine
, displayPoints
, displayLineList
, displayLineListSimple
) where

import Data.List ( minimum
                 , maximum
                 , minimumBy
                 , maximumBy
                 , map
                 , concatMap
                 , concat
                 )

import qualified Graphics.Gloss as G

import Graphics.Rendering.OctoPlot.Base
import Graphics.Rendering.OctoPlot.Gloss.WinUtils

-------------------------------------------------------------------------------

absVec :: (Num a) => (a, a) -> (a, a)
absVec (a, b) = (abs a, abs b)

compareXPos :: (Num a, Ord a) => (a, a) -> (a, a) -> Ordering
compareXPos (x, _) (y, _) = abs x `compare` abs y

compareYPos :: (Num a, Ord a) => (a, a) -> (a, a) -> Ordering
compareYPos (_, x) (_, y) = abs x `compare` abs y

colorToPicColor :: (Float, Float, Float) -> G.Picture -> G.Picture
colorToPicColor (r, g, b) = G.Color $ G.makeColor r g b 1

getAspectRatio :: GridMode -> [(PointMap, Option)] -> AspectRatio
getAspectRatio UseFirst ls = relation $ fst $ head ls
getAspectRatio UseSmallest ls = minimum $ map (relation . fst) ls
getAspectRatio UseBiggest ls = maximum $ map (relation . fst) ls

getMarkerForType :: MarkerType -> Float -> G.Picture
getMarkerForType DotMark r = G.ThickCircle (r / 2.0) r
getMarkerForType CircleMark r = G.Circle r
getMarkerForType ExMark r = G.Pictures
                            [ G.Line [(-r, -r), (r, r)]
                            , G.Line [(-r, r), (r, -r)]
                            ]
getMarkerForType CrosshairMark r = G.Pictures
                                   [ G.Line [(-r, 0), (r, 0)]
                                   , G.Line [(0, r), (0, -r)]
                                   ]
getMarkerForType StarMark r = G.Pictures
                              [ G.Line [(-r, 0), (r, 0)]
                              , G.Line [(0, r), (0, -r)]
                              , G.Line [(-r, -r), (r, r)]
                              , G.Line [(-r, r), (r, -r)]
                              ]
getMarkerForType SquareMark r = G.Polygon
                                [ (r, r)
                                , (-r, r)
                                , (-r, -r)
                                , (r, -r)
                                ]
getMarkerForType DiamondMark r = G.Polygon
                                 [ (0, r)
                                 , (-r, 0)
                                 , (0, -r)
                                 , (r, 0)
                                 ]
getMarkerForType TriangleMark r = G.Polygon
                                  [ (0, r)
                                  , (-r / 2.0, 0)
                                  , (r / 2.0, 0)
                                  ]

prepareToPlot :: (Plotable a)
              => Option
              -> [(Linspace a, Linspace a, Option)]
              -> [(PointMap, Option)]
prepareToPlot gopts ls = nargs
                       where args = map (\(x, y, p) ->
                                            (linspaceToPointMap x y, p))
                                        ls
                             sc = getAspectRatio
                                        (gridModeOption gopts)
                                        args
                             nargs = map (\(p, t) ->
                                             (scale p sc, t))
                                         args

displayAxeLine :: (Float, Float) -> Option -> [G.Picture]
displayAxeLine (w, h) gopts = [ G.Pictures $ px ++ nx ++ py ++ ny
                              , G.Line [(0, 0), (w, 0)]
                              , G.Line [(0, 0), (0, h)]
                              , G.Line [(0, 0), (-w, 0)]
                              , G.Line [(0, 0), (0, -h)]
                                -- Plot contourns:
                              , G.Line [(-w, -h), (-w, h)]
                              , G.Line [(w, -h), (w, h)]
                              , G.Line [(-w, -h), (w, -h)]
                              , G.Line [(w, h), (-w, h)]
                                -- Plot legends for axes:
                              , G.Translate (-w + 50) (h + 50)
                                $ G.Scale 0.5 0.5
                                $ G.Text $ legendOption gopts
                              , G.Translate (-w - 50) (-50)
                                $ G.Scale 0.25 0.25
                                $ G.Rotate 270
                                $ G.Text $ yLabelOption gopts
                              , G.Translate (-50) (-h - 50)
                                $ G.Scale 0.25 0.25
                                $ G.Text $ xLabelOption gopts
                              ]
                            where sx = take (round (w / 5.0) :: Int) [1,2..]
                                  sy = take (round (h / 5.0) :: Int) [1,2..]
                                  s = 2
                                  cvmap = map (`convert` (w, h))
                                  cxmap = map (\(x, y) ->
                                                  G.Line
                                                  [ (x, y - s), (x, y + s) ])
                                  cymap = map (\(x, y) ->
                                                  G.Line
                                                  [ (x - s, y), (x + s, y) ])
                                  px = cxmap $ map (\x -> (x * 5, 0)) sx
                                  nx = cxmap $ map (\x -> (-x * 5, 0)) sx
                                  py = cymap $ map (\x -> (0, x * 5)) sy
                                  ny = cymap $ map (\x -> (0, -x * 5)) sy

-- | A display callback that plots continuous functions.
displayLine :: (PointMap, Option) -> DisplayCallback
displayLine (pm, opts) dm = colorToPicColor (colorToRGB $ penColorOption opts)
                            $ G.Line cords
                          where ps = points pm
                                cords = map (`convert` dm) ps

-- | A display callback that plots specific points.
displayPoints :: (PointMap, Option) -> DisplayCallback
displayPoints (pm, opts) dm = colorToPicColor (colorToRGB $ penColorOption opts)
                              $ G.Pictures circles
                            where ps = points pm
                                  cords = map (`convert` dm) ps
                                  circles = map (\(x, y) ->
                                                      G.Translate x y
                                                    $ getMarkerForType
                                                        (markerTypeOption opts)
                                                        (markerSizeOption opts))
                                                cords

-- | The main display callback, enables plotting, scattering and much more on
-- the same window.
displayLineList :: (Plotable a)
                => [[(Linspace a, Linspace a, Option)]]     -- ^ Data to plot.
                -> Option                                   -- ^ Global options.
                -> [(PointMap, Option) -> DisplayCallback]  -- ^ Display
                                                            -- callbacks to use.
                -> DisplayCallback
displayLineList [] _ _ _ = G.Blank
displayLineList ls gopts disp dm =
                            G.Color (G.makeColor 0 0 0 1)
                          $ G.Scale 0.2 0.2
                          $ G.Pictures
                            [ G.Pictures $ displayAxeLine (maxX, maxY) gopts
                            , G.Pictures
                              $ map (\(a, d) ->
                                        G.Pictures $ map (`d` dm) a)
                                    ndisp
                            ]
                        where nargs = map (prepareToPlot gopts) ls
                              ndisp = zip nargs disp
                              pts = map (`convert` dm)
                                    $ concatMap (points . fst)
                                    $ concat nargs
                              maxX = abs $ fst $ maximumBy compareXPos pts
                              maxY = abs $ snd $ maximumBy compareYPos pts

-- | Like 'displayLineList', but only plots one dataset using only one display
-- callback.
displayLineListSimple :: (Plotable a)
                      => [(Linspace a, Linspace a, Option)]       -- ^ Data to
                                                                  -- plot.
                      -> Option                                   -- ^ Global
                                                                  -- options.
                      -> ((PointMap, Option) -> DisplayCallback)  -- ^ Display
                                                                  -- callback to
                                                                  -- use.
                      -> DisplayCallback
displayLineListSimple ls gopts disp = displayLineList
                                          [ls]
                                          gopts
                                          [disp]
