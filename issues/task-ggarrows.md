# `ggarrow` integration — plan (DONE)

Idea: replace the hand-rolled `geom_segment(..., arrow = arrow_style)` call used for variable
vectors (R/ggbiplot.r:406-414, and duplicated in `examples/ggvector.R`) with
[`ggarrow`](https://teunbrand.github.io/ggarrow/)'s `geom_arrow_segment()`. `ggarrow` is a CRAN
package (Teun van den Brand) built on top of ggplot2's grammar, adding `resect` (shortens the
line from either end so heads don't overlap points/labels — handy for the correlation circle and
crowded variable-name labels), `justify` (where the arrowhead sits along the segment),
`force_arrow`, variable linewidth (could visually encode vector length/quality-of-representation
without a separate legend), and a choice of arrowhead ornaments (`arrow_head_wings()`,
`arrow_head_line()`, `arrow_head_minimal()`, plus `arrow_fins_*()` for a tail decoration) in
place of base `grid::arrow()`. Aesthetic mapping (`colour`, `linewidth`, `alpha`) works the same
way as any ggplot2 geom, so per-group coloring of vectors should carry over without change.

Steps (updated 2026-09-04 — plan changed from gated/Suggests to a direct replacement, per
Michael's call once he'd confirmed the extra options `ggarrow` exposes beyond what's needed here
aren't a problem):

1. [x] Moved `examples/ggvector.R` → `R/ggvector.R`, rewrote it around
   `ggarrow::geom_arrow_segment()` (default ornament `arrow_head_wings()`), fixed a latent bug in
   the old prototype (angle/hjust for label placement ignored a non-zero `origin`; `scale` param
   was declared but never applied), exported it, and added roxygen docs + a runnable example.
   `ggvector()` takes its own `geom.var = c("arrow","text")` so it can draw either or both,
   matching `ggbiplot()`'s existing option.

2. [x] `ggbiplot()`'s two call sites (R/ggbiplot.r: the old arrow block at ~406-414, and the text
   block at ~446-452) now both call `ggvector()` — arrows are still drawn before the ellipse
   layer and labels after, preserving the original z-order (labels need to sit on top of a
   possibly-filled ellipse). Removed now-dead `df.v$angle`/`df.v$hjust` computation and the
   related `globalVariables`-style NULL shim entries in `ggbiplot.r`, since `ggvector()` computes
   those internally via `.data$` pronouns instead.

3. [x] `ggarrow` added to `Imports` (not `Suggests`) — it's the only/default arrow renderer now,
   not an opt-in extra.

4. [x] Verified visually (`devtools::load_all()` + `ggsave()`, wine data): full biplot,
   `geom.var = "arrow"` only, `geom.var = "text"` only, and `ggvector()` used standalone on a
   plain scatterplot all render correctly with matching arrow styling.

5. [x] `devtools::check()` run twice (once on `ggarrow` 0.1.1/CRAN, again after Michael installed
   the GitHub dev version `ggarrow` 0.2.0.9000) — both times **0 errors, 0 warnings, 0 notes**.
   The `geom_arrow_segment()`/`arrow_head_wings()` API is unchanged between those versions (only
   additive new params: `sep`, `distort`), so no compatibility fix was needed. The
   `Config/roxygen2/version` vs. old `RoxygenNote` swap in `DESCRIPTION` (roxygen2 8.1.0 vs. the
   project's previous 7.3.2) did not trip up the check.

6. [x] Investigated an apparent regression under `ggarrow` 0.2.0.9000: standalone `ggvector()`
   demo plots showed arrowheads collapsing into a cluttered mass at the origin. Root cause turned
   out to be the demo script, not the package or the new `ggarrow` version — the demo scaled
   loadings by an arbitrary `3 ×` factor (vector lengths ~0.7–1.6), far shorter than what
   `ggbiplot()` computes internally (~1.4–2.5, scaled to the spread of the point scores via `r` /
   `v.scale` in R/ggbiplot.r), so the arrowhead ornament (sized relative to `linewidth`, not to
   vector length) dominated the short shafts. Fixed the demo scaling and added an `@details` note
   to `ggvector()`'s roxygen docs warning that callers must scale vectors themselves (`ggbiplot()`
   already does this correctly, so it was never affected). Confirmed clean visually afterward,
   including a `resect = 3` example that now correctly shows a small gap pulling the arrowhead
   back from its label, at a properly-scaled vector length.

7. [x] Renamed `ggvector()`'s `resect` argument to `gap` (matching its existing un-prefixed style:
   `size`, `adjust`, `color`), and wired it up as a `ggbiplot()`-level argument `varname.gap`,
   threaded through the arrow-drawing `ggvector()` call site (R/ggbiplot.r). Roxygen docs on both
   functions spell out that the value is in **millimetres** — a fixed physical distance on the
   rendered plot, independent of the PC-score data scale (`obs.scale`/`var.scale` don't affect
   it) — and that it only shortens the drawn arrow, not the label position. Added a
   `varname.gap = 2` usage to the `circle = TRUE` example in `ggbiplot()`'s roxygen docs, a
   NEWS.md entry, and a `issues/varname-gap.R` demo/test script (new top-level `issues/` dir,
   `.Rbuildignore`d, for this kind of notes/test script going forward). `devtools::document()`
   re-run; visually verified (R 4.5.2) that `varname.gap = 3` pulls arrowheads back from their
   endpoint as expected, both with and without `circle = TRUE`.

8. [x] Still need: a comparison example (old `grid::arrow()` vs. new `ggarrow`-based rendering)
   for the pkgdown site/README: Michael's one test so far (`wine.pca`) showed the new default
   arrows look as good as or better than the old ones.
   MF: Tested comparing current README page at friendly.github.io. The new arrows in examples are all
   very pretty by default, but quite a bit thicker than the previous version. Suggests a need to
   control thickness of the arrows.
   Addressed: after comparing panels (old grid::arrow() / new ggarrow default / thinner / a
   arrow_head_line() recipe) on both wine and crime data, Michael's call was to keep
   arrow_head_wings() (prefers its look to arrow_head_line()) but lower the default shaft
   linewidth. `ggvector()`'s (and so `ggbiplot()`'s) default `linewidth` is now `0.9`, down
   from `1.4` — this is an actual default-behavior change, not just a `vector.args` recipe;
   `vector.args = list(linewidth = 1.4)` restores the old shaft weight. See
   `issues/arrow-tests.R` (uses the `crime` dataset — matches what README.Rmd actually shows,
   not `wine`) for the comparison and to explore further.
   Still open: an actual before/after comparison figure hasn't been added to the README/site
   itself.

   Also found while building the crime comparison (unrelated to arrows): replaying
   README.Rmd's `crime-biplot1` chunk as-is against the current package produced plain
   colored points with no state-abbreviation labels, and `crime-biplot1` also had
   `ellipse.level = 0.5` (not a real `ggbiplot()` argument — silently absorbed by `...`)
   instead of `ellipse.prob`. Both fixed 2026-09-04: added `geom.ind = "text"` to both crime
   chunks, corrected `ellipse.level` → `ellipse.prob`, and re-knit README.Rmd via
   `devtools::build_readme()` (which installs the current dev version first, so
   `packageVersion()` picks up 0.6.5 instead of the stale 0.6.2 that was in the system
   library — that stale-version mismatch was the actual source of the "0.6.2 vs 0.6.5"
   README.md drift noticed earlier the same day). Pushed at commit `35c2631`.
