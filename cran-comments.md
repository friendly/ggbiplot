## Test environments

* Windows 11, R 4.6.1 (2026-06-24 ucrt), local `devtools::check(cran = TRUE, remote = TRUE)`
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

## Submission notes

This is an update from the CRAN version (0.6.2) to 0.6.5, rolling up the following releases
(from NEWS.md):

### Version 0.6.5

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

### Version 0.6.4

- Documented solution to 'scale_color_discrete() produces two legends' #2
- Fix glitch with axis labels
- `ggbiplot` gains `geom.ind` and `geom.var` arguments for more flexible handling of the
  geometries used to display the observation points and variable labels.

### Version 0.6.3

- Fix axis label spacing
- `ggbiplot` gains a `clip` argument to control if points, labels, etc. are clipped to the
  axis limits.
