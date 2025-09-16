## Version 0.6.5

- Now Depends R (>= 4.1.0) to handle |>
- Added lda() objects to those handled by reflect()

## Version 0.6.4

- Documented solution to 'scale_color_discrete() produces two legends' #2
- Fix glitch with axis labels
- `ggbiplot` gains `geom.ind` and `geom.var` arguments for more flexlible handling of the geometries used to display the
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


