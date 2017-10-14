# OctoPlot Haskell Library #

Plotting with Haskell! this library was designed to make plotting easy and
fast.

## Description ##

OctoPlot is a plotting library designed to be easy to used and fast to
implement. Originally designed for the [`hs-automata`][pkg-hs-automata] package
(the Automata library for Haskell), this library includes support for
plotting, scattering and simulations by using just one function (per-task)!

The interface of this library was based on the MATLAB / GNU-Octave plotting
functions. By using the [`gloss`][pkg-gloss] package (the Gloss library) this
library can easily plot and scatter functions.

## Examples ##

### Plotting the sin function ###

```haskell
import Graphics.Rendering.OctoPlot

-- defined only for convenience with following examples:
mySignal :: Float -> Float
mySignal = sin

x = linspace (-2 * pi) 200.0 (2 * pi)
y = mySignal <$> x

main = plot x y mkDefaultOption
```

### Changing the legend ###

```haskell
import Graphics.Rendering.OctoPlot

-- defined only for convenience with following examples:
mySignal :: Float -> Float
mySignal = sin

x = linspace (-2 * pi) 200.0 (2 * pi)
y = mySignal <$> x

myOptions = mkDefaultOption { legendOption = "The sin function" }

main = plot x y myOptions
```

### Scattering ###

```haskell
import Graphics.Rendering.OctoPlot

x :: Linspace Float
x = linspace (-2 * pi) 20.0 (2 * pi)
y :: Linspace Float
y = (\x -> abs (sin x) * x) <$> x

opts :: Option
opts = mkDefaultOption { legendOption = "Scattering example" }

main = scatter x y opts
```

### Simulation ###

```haskell
import Graphics.Rendering.OctoPlot

data Model = Model { counter :: !Int }

transitionF :: Model -> Model
transitionF m = Model { counter = counter m + 1 }

displayF :: Model -> Picture
displayF Model { counter = c } = Text $ show c

main = simulate 1
                Model { counter = 1 }
                transitionF
                displayF
```

## Dependencies ##

* The Gloss library: Cabal package [`gloss`][pkg-gloss].
* The [`containers`][pkg-containers] package.
* A Haskell 2010 compiler.

## Installing ##

This library is cabal-ized, so you can just use:

```sh
cabal configure
cabal build
cabal install
```

This library also has support for Haddock:

```sh
cabal haddock --html
```

Also, this library is on [Hackage][hackage] as
[`hs-octoplot`][pkg-hs-octoplot]:

```sh
cabal update
cabal install hs-octoplot
```

## Documentation ##

You can see the full documentation on [Hackage][my-pkg-docs] or the basic
tutorial on [the `docs/` folder][docs-folder].

## Alternatives ##

The [`Chart`][pkg-chart] package provides a full-featured support for complex
plotting (like histograms, vector-plotting and more).

The [`easyplot`][pkg-easyplot] package provides a GNUPlot backend and similar
functions.

The [`matplotlib`][pkg-matplotlib] is a full-featured plotting library based
on the Python's `matplotlib` module.

None of them provides some of the functions heavily used by the main user of
this library: [`hs-automata`][pkg-hs-automata].

## License ##

[LGPL-3.0](https://www.gnu.org/licenses/lgpl.txt)

[pkg-hs-automata]: http://hackage.haskell.org/package/hs-automata
[pkg-gloss]: http://hackage.haskell.org/package/gloss
[pkg-containers]: http://hackage.haskell.org/package/containers
[pkg-hs-octoplot]: http://hackage.haskell.org/package/hs-octoplot
[pkg-chart]: http://hackage.haskell.org/package/Chart
[pkg-easyplot]: http://hackage.haskell.org/package/easyplot
[pkg-matplotlib]: http://hackage.haskell.org/package/matplotlib

[hackage]: http://hackage.haskell.org/
[my-pkg-docs]: http://hackage.haskell.org/package/hs-octoplot-0.1.0.0/
[docs-folder]: docs/
