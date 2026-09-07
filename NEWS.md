## Version 0.6.6

- `ggvector()` gains an `angle` argument for the text labels. Default `NULL` computes each
  label's angle from its own vector's direction, as before; supply a fixed number (e.g. `0`
  for horizontal labels) to use the same angle for every label instead. Exposed on `ggbiplot()`
  as `varname.angle`.

## Version 0.6.5

- Now Depends R (>= 4.1.0) to handle `|>`
- Added `lda()` objects to those handled by `reflect()`
- Illustrate reflection in ggbiplot examples
- Added `ggvector()`, exported, to draw labeled vectors from a common origin using
  `ggarrow::geom_arrow_segment()`. `ggbiplot()` now uses this internally to draw variable
  vectors and their labels, replacing the previous `geom_segment()` + `grid::arrow()` code.
  `ggarrow` is now a package dependency (Imports).
- `ggbiplot()` gains a `varname.gap` argument to pull variable-vector arrowheads back
  (in mm) from their true endpoint, e.g. to keep them clear of the correlation circle or
  of crowded labels. Exposed on `ggvector()` as `gap`.
- `ggvector()`'s (and so `ggbiplot()`'s) default arrow `linewidth` is now `0.9`, down from
  `1.4` (which matched the shaft width of the old `grid::arrow()`-based rendering).
  `ggarrow`'s default `arrow_head_wings()` ornament reads visually heavier than the old
  plain triangular arrowhead at the same linewidth, so `1.4` now looks noticeably thicker
  than the pre-`ggarrow` arrows did; `0.9` was chosen to look comparable. Use
  `vector.args = list(linewidth = 1.4)` to get the old shaft weight back.
- `ggbiplot()` gains a `vector.args` argument: a named list forwarded to the `ggvector()`
  call that draws the variable-vector arrows, e.g. `list(arrow_head = ggarrow::arrow_head_line())`
  for a different arrowhead shape. Anything not matched by a `ggvector()` argument passes on
  to `ggarrow::geom_arrow_segment()` (`justify`, `force_arrow`, `sep`, `distort`, ...).

## Version 0.6.4

- Documented solution to 'scale_color_discrete() produces two legends' #2
- Fix glitch with axis labels
- `ggbiplot` gains `geom.ind` and `geom.var` arguments for more flexible handling of the geometries used to display the
observation points and variable labels.

## Version 0.6.3

- Fix axis label spacing
- `ggbiplot` gains a `clip` argument to control if points, labels, etc. are clipped to the axis limits.

## Version 0.6.2

This is a modest upgrade to the initial release, adding a number of features.

- corrected small bugs in `ggbiplot() and `ggscreeplot()`
- `reflect()` now also reflects supplementary variables from `FactoMineR::PCA()`
- added support for `ade4::dudi.pca()`
- `ggbiplot()` gains an `axis.title` argument
- `ggscreeplot()` gains `color`, `shape`, `linetype` and `linewidth` arguments
- Added `get_SVD()` intended the simplify the interface to various PCA functions.
- Now use `get_SVD()` in `ggbiplot()` and `ggscreeplot()`
- Extend some examples, requiring Depends:ggplot2

## Version 0.6.1

- Revised documentation for `ggscreeplot()`
- `data(crime)` used in README giving a more complete example.
- Fixed links reported as 301s
- Published pkgdown site

## Version 0.6.0

- Use `geom_polygon()` rather than `geom_path()` for ellipses to allow them to be filled.
- Added `var.factor` argument to expand or reflect the variable vectors
- Moved points/labels code earlier so ellipses and variable vectors are not obscured
- Replaced internal calculation of ellipses with `stat_ellipse()`
- Now allow ellipses to be filled (`geom_polygon()`) or unfilled (`geom_path()`)
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


