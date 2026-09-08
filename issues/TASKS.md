# ggbiplot — development tasks

Broken out from the cross-package working list in `C:\Dropbox\R\TASKS-all.md` (2026-09-08).
Update here as items are finished; sync back to the main list only if it's useful to see
ggbiplot status at a glance across packages.

Package is small (7 files in `R/`): `ggbiplot.R`, `ggvector.R`, `get_SVD.R`, `reflect.R`,
`ggscreeplot.R`, `crime.R`/`wine.R` (data docs). Version 0.6.6 in dev (0.6.5 is the latest on
CRAN, 0.6.6 not yet submitted).

## `ggarrow` integration — DONE

Migrated variable-vector drawing from hand-rolled `grid::arrow()` to
`ggarrow::geom_arrow_segment()`, shipped in 0.6.5. Full step-by-step history moved to
`issues/task-ggarrows.md` (kept for reference, not an active task).

## 0.6.6 changes (not yet submitted to CRAN)

**2026-09-07: `ggvector()` gains `angle` (and `ggbiplot()` a matching `varname.angle`)** —
previously the text-label angle always tracked each vector's own direction, no way to override,
forcing users to draw a separate plain `geom_text()` for e.g. horizontal labels (hit in
Vis-MLM-book's `C:\R\Projects\Vis-MLM-book\R\diabetes\diabetes-mds.R`). `angle = NULL` (default)
keeps the old per-vector behavior; a fixed number (e.g. `0`) draws every label at that angle.
Commits `4f80f3e` (code+docs) and `81d2171` (pkgdown rebuild), pushed. Bumped to 0.6.6
(Date 2026-09-07); `devtools::check(cran = TRUE, remote = TRUE)` 0/0/0.
Follow-up not yet done: update `diabetes-mds.R` itself to use `angle = 0` instead of its
separate-`geom_text()` workaround.

**2026-09-07: `ggbiplot()`'s `clip` argument now defaults to `"off"`** (was `"on"`) — a long
variable name, or one pushed outward by a large `varname.adjust`, can extend past the panel
edge (ggplot2 computes axis limits from each layer's anchor coordinates only, not the rendered
text's extent); `"off"` lets that overflow draw into the margin instead of silently truncating
it. Commit `a0d4a15`, pushed.

- [ ] Not yet submitted to CRAN — small enough to bundle with other fixes before the next
  submission, or submit alone.

## CRAN resubmission (0.6.5) — DONE, accepted 2026-09-07

Closed the gap since CRAN's 0.6.2 (three versions behind). Triggered because Vis-MLM-book Ch05
depends on the `ggarrow` arrow rewrite; the earlier tiny-text figure scare was confirmed
unrelated to `ggbiplot` (root cause: `FactoMineR::plot.PCA()` calling `showtext::showtext_auto()`
globally, fixed upstream in FactoMineR 2.17). No version bump was needed (0.6.5 already exceeded
CRAN's 0.6.2). Status:
- [x] `.r` → `.R` filename rename (`ggbiplot.R`, `ggscreeplot.R`) — commit `3e63c1f`, pushed.
- [x] `devtools::check(cran = TRUE, remote = TRUE)` → 0 errors, 0 warnings, 0 notes (after
  bumping the stale `Date:` field 2025-09-16 → 2026-09-06).
- [x] `urlchecker::url_check()` clean; `devtools::spell_check()` only flags legit technical terms.
- [x] Reverse deps (`CoDaLoMic`, `heplots`, `pPCA`) checked via `myutil::release_revdep()` — 0
  new problems.
- [x] `cran-comments.md` rewritten and assembled (win-builder devel clean, revdep summary, full
  NEWS.md text for 0.6.3-0.6.5).
- [x] Win-builder check: clean.
- [x] Release issue opened: [friendly/ggbiplot#6](https://github.com/friendly/ggbiplot/issues/6)
  (via `usethis::use_release_issue()`'s internals called directly against `cfg$origin`, to avoid
  both an unwanted version bump and `usethis`'s fork-remote ambiguity — see note below).
- [x] Submitted via `devtools::submit_cran()`; one round-trip needed (CRAN flagged a 301 on the
  new pkgdown badge URL missing a trailing slash — fixed, resubmitted, accepted).
- [x] **Accepted on CRAN 2026-09-07.**
- [x] GitHub release published: [v.0.6.5](https://github.com/friendly/ggbiplot/releases/tag/v.0.6.5),
  cumulative notes covering 0.6.3-0.6.5 since the last GH release (`v.0.6.2`).
- [ ] Optional polish, not done: the before/after arrow-comparison figure for README/pkgdown
  (see `issues/task-ggarrows.md`, step 8).

**Repo quirk found along the way**: `friendly/ggbiplot` is still registered on GitHub as a fork
of `vqv/ggbiplot` (dormant since 2015), with no `upstream` remote — this blocks any
`usethis` GitHub-remote-aware function (`use_release_issue()`, `use_github_release()`, etc.) run
non-interactively, and even interactively `use_github_release()`'s default role would resolve to
the fork's parent, not `friendly/ggbiplot`. Worked around for now by calling `usethis`'s internal
checklist/release-notes builders directly against `github_remote_config()$origin`. Added a local
`upstream` remote (fetch-only, pointing at `vqv/ggbiplot`) to match GitHub's own fork-parent
record, but the real fix — asking GitHub Support to detach the fork relationship, since
`friendly/ggbiplot` is the actively maintained repo now — hasn't been requested yet.

## Open GitHub issues

- [ ] #4 Scaling arguments (`pc.biplot`, `obs.scale`, `var.scale`) — reporter argues the
  `pc.biplot` branch in `ggbiplot()` (R/ggbiplot.R:306-308) only scales `df.u` by `nobs.factor`,
  not `df.v` as the docs claim, and that the `obs.scale`/`var.scale` defaults look swapped
  relative to the `@details` description of α. Needs a from-scratch derivation check against
  Gabriel's biplot definitions before changing defaults (this is math-correctness, not style).

- [ ] #3 Variable name modification — wants `expression()`/`parse = TRUE` support in the
  `geom_text()` variable-name labels (R/ggbiplot.R:446-452) so labels can carry Greek letters
  and sub/superscripts (e.g. `expression(Phi["PSII"])`). Reporter's workaround: assign
  `expression()` vectors into `rownames(pcobj$rotation)` directly. A clean fix would add a
  `varname.parse` (or similar) argument threaded to `geom_text(parse = TRUE)`.

- [x] #2 `scale_color_discrete()` produces two legends — CLOSED 2026-09-06: general ggplot2
  legend-merging behavior (aesthetics only merge when named alike), not a ggbiplot bug.
  Documented as of 0.6.4 (`ggbiplot.R:80-93`, workaround example at `ggbiplot.R:230`); closed
  with an explanatory comment rather than a code change.

## Selective point labeling ("noteworthy" points) — in progress

- [ ] Implement a method for selective point identification/labeling, as general as possible,
  matching what's available in `car::showLabels()`. Belongs here in `ggbiplot` (not `heplots`,
  which doesn't use `ggplot2` methods). See `dev/peng-out-test.R` for the original manual
  workaround this is meant to replace.

  **Survey done (2026-09-07, with Claude) — what actually exists:**
  - `heplots::noteworthy(x, y, n, method, level, ...)` — the selection logic (not drawing) is
    fully shipped, exported, documented, in **heplots 1.7.4** (2025-04-15, `R/noteworthy.R`).
    Extends `car::showLabels()`'s `method` options (`"mahal"`, `"dsq"`, `"x"`, `"y"`, `"r"`,
    `"ry"`, or a supplied numeric/case-ID vector) — returns row indices only. Not yet consumed
    anywhere else in `heplots` itself (`cqplot()` still calls `car::showLabels()` directly).
  - The ggplot2-layer wrapper was the actual unfinished part in `heplots` (never exported); see
    `heplots/dev/stat_noteworthy.R`, `heplots/dev/noteworthy-notes.md`.
  - In Vis-MLM-book, only the base function ever got used, and only via the manual pattern —
    the stat was never actually adopted (Ch. 11, `test/Gina-mahalanobis.Rmd`,
    `R/penguin/peng-out-test.R`).
  - **Implication for `ggbiplot`**: reuse `heplots::noteworthy()` for selection; finish the
    `StatNoteworthy`/`stat_noteworthy()` ggplot2-layer mechanism; give `ggbiplot()`/`ggvector()`
    a `labels.method`/`labels.n`-style argument so users don't have to hand-roll the label
    vector as in `dev/peng-out-test.R`.

  **Prototype built 2026-09-07/08 (with Claude)**: `dev/stat_noteworthy.R` (`StatNoteworthy`
  ggproto + `stat_noteworthy()` constructor, production-style roxygen doc block) +
  `dev/test-noteworthy.R` (bug check + 14 worked examples, later extended with facet/group-
  scoping tests). Verified the heplots draft's documented `label=` bug in the general case (a
  short externally-computed label vector fails ggplot2's pre-filter aesthetic-length check —
  see `dev/ggextenders-noteworthy.md`, notes from the original
  [ggplot-extension-club discussion #91](https://github.com/ggplot2-extenders/ggplot-extension-club/discussions/91)
  that `friendly` opened; also documents the deliberate `compute_panel()` (pools groups sharing
  a panel) vs `compute_group()` (per-group) design choice, and a real repel/label-blanking gap
  for dense plots flagged by ggpp's author). Committed `21d0b1f`; further facet/group test work
  and the `ggextenders-noteworthy.md` notes done on laptop, not yet committed as of 2026-09-08.
  Still not wired into `ggbiplot()`'s own argument surface — that's the remaining step.

## `reflect()` generalization — S3 generic DONE (2026-09-08), other ideas still open

**Done**: `reflect()` converted to an S3 generic (`UseMethod`). The existing
`prcomp`/`princomp`/`PCA`/`lda` behavior is unchanged (pure refactor of the old if/else chain
into one method per class), and it gains `reflect.data.frame()`, `reflect.matrix()`, and
`reflect.list()` methods that negate the given `columns` (by name or index) directly, plus a
`reflect.default()` with a clear error naming the unsupported class. This is what candisc's own
TASKS notes were hoping for — a generic other packages can add `reflect.<theirclass>()` methods
to, rather than needing changes here. Argument kept as `pcobj` (rename to `object` proposed but
deliberately held off, to avoid a breaking change for named-argument callers). Verified all
methods + error paths manually; `devtools::check()` 0/0/0. NEWS.md entry added under 0.6.6 (not
yet submitted to CRAN). The reciprocal-transform idea (`mpg` → `1/mpg` = gallons/mile) was
explicitly ruled out of scope — that's a different operation than reflection (sign-flip), not
implemented.

**Also done (2026-09-08)**: added a `scale = 1` argument to every method — selected columns
become `-scale * columns` instead of just `-1 * columns` (default `scale = 1` reproduces the
old behavior exactly). Useful to stretch/shrink variable vectors in a biplot at the same time
as reflecting them, as in Vis-MLM Fig 9.7's `cars.pca$rotation <- -2.5 * cars.pca$rotation`
(see "Collinearity biplots" below) — now just `reflect(cars.pca, scale = 2.5)`. A negative
`scale` scales without reflecting. Verified manually (including negative-scale and `list`
forwarding); `devtools::check()` still 0/0/0.

**Still open**: are there other examples of "parallel structures" (beyond a list of
data.frames/matrices) worth their own `reflect.<class>()` method?

## Collinearity biplots

It would be useful here to attempt a `ggbiplot` version of Fig 9.7 from Vis-MLM (collinearity
biplot of the `cars` data, last two PCA dimensions, `factoextra::fviz_pca_biplot()` +
`geom_text_repel()` for outlier labels — ties into the "Selective point labeling" work above).
Source: `C:\R\Projects\Vis-MLM-book\09-collinearity-ridge.qmd` lines 699-730
(`fig-cars-collin-biplot`); underlying R script is
`C:\R\Projects\Vis-MLM-book\R\cars-colldiag.R` (not `cars-collin.R`), lines ~106-137.
  
## Collinearity biplots

It would be useful here to attempt a `ggbiplot` version of Fig 9.7 from Vis-MLM?
Code is there in `R/cars-collin.R`

## In-repo TODOs / loose ends

- `R/ggbiplot.R:459` — `# TODO: Add a second set of axes` (never implemented).
- `R/ggbiplot.R:246-281` — large commented-out block duplicating what `get_SVD()` now does;
  candidate for deletion since `get_SVD()` (added v0.6.2) superseded it.
- `R/ggbiplot.R:357-371` — commented-out original points/labels logic, superseded by the
  `geom.ind` branch below it; same for `R/ggbiplot.R:453-457` (commented-out legend-naming code).
- `discussion/` folder has survey notes on competing biplot packages (`biplot-packages.Rmd/.xlsx`,
  `biplotEZ-ex.R`, `biplot2d3d-ex.R`, `tidy_pca.R`) — background research, not actionable but
  useful context for feature comparisons.
