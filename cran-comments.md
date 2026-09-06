## Test environments

* Windows 11, R 4.6.1 (2026-06-24 ucrt), local `devtools::check(cran = TRUE, remote = TRUE)`
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

This is an update from the CRAN version (0.6.2) to 0.6.5, rolling up several releases'
worth of changes (see NEWS.md for full details of 0.6.3, 0.6.4, 0.6.5). Highlights:

- `ggbiplot()` gains `geom.ind`/`geom.var`, `clip`, `varname.gap`, and `vector.args`
  arguments for more flexible control of the plotted geometries.
- Variable vectors are now drawn with `ggarrow::geom_arrow_segment()` instead of
  `geom_segment()` + `grid::arrow()`. `ggarrow` is a new Imports dependency.
- `reflect()` gains support for `MASS::lda()` objects.
- Now Depends R (>= 4.1.0) for the native pipe (`|>`).

No changes needed to reverse dependencies.
