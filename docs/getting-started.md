# Getting Started with OctoPlot #

OctoPlot have a small API, focused on an easy-to-use experience.

## Starting ##

First, it's required to have installed OctoPlot:

```sh
cabal update
cabal install hs-octoplot
```

Then, let's create a file called `OctoTest.hs` and put this content in it:

```haskell
import Graphics.Rendering.OctoPlot
```

This file will be used as boilerplate for later programs.

## 2D Plotting ##

Let's start by plotting the sin function: in the `OctoTest.hs` file write:

```haskell
-- OctoTest.hs
import Graphics.Rendering.OctoPlot

x = linspace 0.0 100.0 (2 * pi) :: Linspace Float
y = sin <$> x

main = plot x y mkDefaultOption
```

Simple: the `linspace` function takes three arguments like the GNU Octave
`linspace` function but with a sighly changed order:

        linspace :: (Plotable a) => a -> a -> a -> Linspace a
        linspace base n limit

This creates a `Linspace` of `n` linearly separated elements between `base`
and `limit`. It's required that `base < limit`.

A `Linspace` it's a list of some `Plotable` element so you can use normal
list operations on them (in the example, we use applicative functors to apply
the `sin` function to all Linspace's elements).

`Linspace`s can be created by hand too as we will see later on the scattering
section.

The `plot` function have a very simple type:

        plot :: (Plotable a) => Linspace a -> Linspace a -> Option -> IO ()
        plot x y options

This will plot two linspaces as the X and Y coordinates of some continuous
function.

### Customizing ###

The last parameter for the `plot` function was an `Option`, but, what is an
`Option`?

The `Option` type enables customization of the resulting plot. There are
two types of options (not in types): *local options* and *global options*.

The *local options* are applied only to **one** plot, while the global ones
are applied to **all** plots. This only makes sense on the `N` family of
functions: for now, let's see just the `plot` counterpart `plotN`:

        plotN :: (Plotable a)
              => [(Linspace a, Linspace a, Option)]
              -> Option
              -> IO ()
        plotN datasets global_options

In this function (which plots *multiples* continuous functions instead of one)
the `Option`s inside the list are the *local options* and them apply only to
that plot. The `Option` argument is the global option and it's applied to all
plots.

The `Option` type have a lot of fields, but the most useful all are:

* `penColorOption :: Color` (local mode, see `Color` in
  `Graphic.Rendering.OctoPlot.Base`): the pen color (color of the line or the
  marks) used when drawing.
* `legendOption :: String` (global mode): The legend (title) of this plot.
* `xLabelOption :: String` (global mode): The label for the X axis.
* `yLabelOption :: String` (global mode): The label for the Y axis.
* `markerSizeOption :: Float` (local mode): The size for the marker (only
  when scattering).

**Note**: Theorically, all options *should* be supported on all modes (both
*local* and *global*) but for implementation details, most options only work
on one mode.

These options also apply for scattering.

Now, we can use this with the `mkDefaultOption` function which returns the
default set of options to customize our original example! lets make the plotted
function's line color magenta:

```haskell
-- OctoTest.hs
import Graphics.Rendering.OctoPlot

x = linspace 0.0 100.0 (2 * pi) :: Linspace Float
y = sin <$> x

myOptions = mkDefaultOption { penColorOption = Magenta }

main = plot x y myOptions
```

Easy!

### Multiplotting ###

We can also plot multiples functions at the same time using the
already-introduced `plotN` function:

```haskell
-- OctoTest.hs
import Graphics.Rendering.OctoPlot

x = linspace 0.0 100.0 (2 * pi) :: Linspace Float
y = sin <$> x
z = cos <$> x

xopt = mkDefaultOption { penColorOption = Red }
zopt = mkDefaultOption { penColorOption = Blue }

main = plotN [(x, y, xopt), (x, z, zopt)] mkDefaultOption
```

In this case we do not modify the global options, but instead modified only
the local ones to make the plots of different colors. We can also set the
plot's title by using the `legendOption` option:

```haskell
-- OctoTest.hs
import Graphics.Rendering.OctoPlot

x = linspace 0.0 100.0 (2 * pi) :: Linspace Float
y = sin <$> x
z = cos <$> x

xopt = mkDefaultOption { penColorOption = Red }
zopt = mkDefaultOption { penColorOption = Blue }
gopt = mkDefaultOption { legendOption = "This is my plot!" }

main = plotN [(x, y, xopt), (x, z, zopt)] gopt
```

## Scattering ##

We can also scatter a dataset, with this, we can display the *points* of the
two linspaces instead of a continuous line between them: just use the
very similar functions `scatter` and `scatterN` (they have the same type than
it's `plot` and `plotN` counterparts):

```haskell
-- OctoTest.hs
import Graphics.Rendering.OctoPlot

signal :: Float -> Float -> Float
signal s a = abs (sin a) ** s * a

x = linspace (-2 * pi) 10.0 0.0 :: Linspace Float
y = signal 2 <$> x
z = linspace 0.0 10.0 (2 * pi) :: Linspace Float
w = signal 3 <$> z

xopt = mkDefaultOption { penColorOption = Red }
zopt = mkDefaultOption { penColorOption = Blue }
gopt = mkDefaultOption { legendOption = "This is my plot!" }

main = scatterN [(x, y, xopt), (z, w, zopt)] gopt
```

## Advanced functions ##

OctoPlot also supports more advanced functions, like `simulate` and
`scatterAndPlot`. You can see them and some examples on the OctoPlot
documentation.

## Note about mixing calls ##

If you try to mix calls to all visualization functions of this library, the
program will only execute the first: this is because the handler called when
the window is closed terminates the program.

So, the following program will not work as expected:

```haskell
main = do
    plot x y mkDefaultOption
    scatter x y mkDefaultOption -- This will never be executed!
```
