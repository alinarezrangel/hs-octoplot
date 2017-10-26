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
    Module       : Graphics.Rendering.OctoPlot
    Description  : Imports all OctoPlot base.
    Copyright    : (c) 2017 Alejandro Linarez
    License      : LGPL-3
    Maintainer   : alinarezrangel@gmail.com
    Stability    : stable
    Portability  : portable
-}
module Graphics.Rendering.OctoPlot
( module Graphics.Rendering.OctoPlot.Base
, module Graphics.Rendering.OctoPlot.Gloss
, module Graphics.Rendering.OctoPlot.EasyPlot
) where

import Graphics.Rendering.OctoPlot.Base
import Graphics.Rendering.OctoPlot.Gloss (scatter, scatterN, simulate)
import Graphics.Rendering.OctoPlot.EasyPlot (plot, plotN)
