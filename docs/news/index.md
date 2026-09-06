# Changelog

## Version 0.6.5

- Now Depends R (\>= 4.1.0) to handle `|>`
- Added `lda()` objects to those handled by
  [`reflect()`](https://friendly.github.io/ggbiplot/reference/reflect.md)
- Illustrate reflection in ggbiplot examples
- Added
  [`ggvector()`](https://friendly.github.io/ggbiplot/reference/ggvector.md),
  exported, to draw labeled vectors from a common origin using
  [`ggarrow::geom_arrow_segment()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.html).
  [`ggbiplot()`](https://friendly.github.io/ggbiplot/reference/ggbiplot.md)
  now uses this internally to draw variable vectors and their labels,
  replacing the previous
  [`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html) +
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) code. `ggarrow`
  is now a package dependency (Imports).
- [`ggbiplot()`](https://friendly.github.io/ggbiplot/reference/ggbiplot.md)
  gains a `varname.gap` argument to pull variable-vector arrowheads back
  (in mm) from their true endpoint, e.g. to keep them clear of the
  correlation circle or of crowded labels. Exposed on
  [`ggvector()`](https://friendly.github.io/ggbiplot/reference/ggvector.md)
  as `gap`.
- [`ggvector()`](https://friendly.github.io/ggbiplot/reference/ggvector.md)’s
  (and so
  [`ggbiplot()`](https://friendly.github.io/ggbiplot/reference/ggbiplot.md)’s)
  default arrow `linewidth` is now `0.9`, down from `1.4` (which matched
  the shaft width of the old
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html)-based rendering).
  `ggarrow`’s default `arrow_head_wings()` ornament reads visually
  heavier than the old plain triangular arrowhead at the same linewidth,
  so `1.4` now looks noticeably thicker than the pre-`ggarrow` arrows
  did; `0.9` was chosen to look comparable. Use
  `vector.args = list(linewidth = 1.4)` to get the old shaft weight
  back.
- [`ggbiplot()`](https://friendly.github.io/ggbiplot/reference/ggbiplot.md)
  gains a `vector.args` argument: a named list forwarded to the
  [`ggvector()`](https://friendly.github.io/ggbiplot/reference/ggvector.md)
  call that draws the variable-vector arrows,
  e.g. `list(arrow_head = ggarrow::arrow_head_line())` for a different
  arrowhead shape. Anything not matched by a
  [`ggvector()`](https://friendly.github.io/ggbiplot/reference/ggvector.md)
  argument passes on to
  [`ggarrow::geom_arrow_segment()`](https://teunbrand.github.io/ggarrow/reference/geom_arrow_segment.html)
  (`justify`, `force_arrow`, `sep`, `distort`, …).

## Version 0.6.4

- Documented solution to ‘scale_color_discrete() produces two legends’
  [\#2](https://github.com/friendly/ggbiplot/issues/2)
- Fix glitch with axis labels
- `ggbiplot` gains `geom.ind` and `geom.var` arguments for more flexible
  handling of the geometries used to display the observation points and
  variable labels.

## Version 0.6.3

- Fix axis label spacing
- `ggbiplot` gains a `clip` argument to control if points, labels, etc.
  are clipped to the axis limits.

## Version 0.6.2

CRAN release: 2024-01-08

This is a modest upgrade to the initial release, adding a number of
features.

- corrected small bugs in `ggbiplot() and`ggscreeplot()\`
- [`reflect()`](https://friendly.github.io/ggbiplot/reference/reflect.md)
  now also reflects supplementary variables from
  [`FactoMineR::PCA()`](https://rdrr.io/pkg/FactoMineR/man/PCA.html)
- added support for
  [`ade4::dudi.pca()`](https://adeverse.github.io/ade4/reference/dudi.pca.html)
- [`ggbiplot()`](https://friendly.github.io/ggbiplot/reference/ggbiplot.md)
  gains an `axis.title` argument
- [`ggscreeplot()`](https://friendly.github.io/ggbiplot/reference/ggscreeplot.md)
  gains `color`, `shape`, `linetype` and `linewidth` arguments
- Added
  [`get_SVD()`](https://friendly.github.io/ggbiplot/reference/get_SVD.md)
  intended the simplify the interface to various PCA functions.
- Now use
  [`get_SVD()`](https://friendly.github.io/ggbiplot/reference/get_SVD.md)
  in
  [`ggbiplot()`](https://friendly.github.io/ggbiplot/reference/ggbiplot.md)
  and
  [`ggscreeplot()`](https://friendly.github.io/ggbiplot/reference/ggscreeplot.md)
- Extend some examples, requiring Depends:ggplot2

## Version 0.6.1

CRAN release: 2023-12-17

- Revised documentation for
  [`ggscreeplot()`](https://friendly.github.io/ggbiplot/reference/ggscreeplot.md)
- `data(crime)` used in README giving a more complete example.
- Fixed links reported as 301s
- Published pkgdown site

## Version 0.6.0

- Use
  [`geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.html)
  rather than
  [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  for ellipses to allow them to be filled.
- Added `var.factor` argument to expand or reflect the variable vectors
- Moved points/labels code earlier so ellipses and variable vectors are
  not obscured
- Replaced internal calculation of ellipses with
  [`stat_ellipse()`](https://ggplot2.tidyverse.org/reference/stat_ellipse.html)
- Now allow ellipses to be filled
  ([`geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.html))
  or unfilled
  ([`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html))
- Added hex logo
- Added iris example to README
- Removed Imports: dplyr, purrr as no longer needed

## Version 0.56

- Fixed many documentation errors and warnings
- Use roxygen2 for documentation
- added `varname.color` to replace fixed `muted("red")`
- tweaked arrow style
- increased default thickness of variable vectors
- add `point.size` argument
