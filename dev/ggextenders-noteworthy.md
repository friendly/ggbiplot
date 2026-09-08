# ggplot-extension-club discussion #91: notes relevant to `stat_noteworthy()`

Source: <https://github.com/ggplot2-extenders/ggplot-extension-club/discussions/91>
("ggplot2: Labeling noteworthy points in scatterplots (`geom_noteworthy`)", opened by
@friendly 2025-04-16, 6 top-level comments + replies through 2025-04-20).

Summary of what's in the thread, organized by relevance to `dev/stat_noteworthy.R`.

## Origin & first sketch

@friendly opened the discussion wanting a `geom_noteworthy()` modeled on
`car::showLabels()`'s `id = list(n, method, ...)` interface, having already built
`heplots::noteworthy(x, y, n, method, ...)` (heplots 1.7.4) as the pure selection function
(returns row indices only). Motivating example: labeling Mahalanobis-D²-unusual departments
in the `Guerry` data, previously requiring a manual subset-and-`geom_label_repel()` workaround.

@teunbrand posted the first working sketch:

```r
StatNoteworthy <- ggproto(
  "StatNoteworthy", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, method = "dsq", n = 10) {
    idx <- heplots::noteworthy(x = data$x, y = data$y, n = n, method = method)
    data[idx, , drop = FALSE]
  }
)
```

Note the differences from what's now in `dev/stat_noteworthy.R`: this sketch uses
**`compute_group()`**, with defaults `method = "dsq"`, `n = 10`. The shipped version changed
both the scope (`compute_panel()`) and the defaults (`method = "mahal"`, `n = 5`) during
implementation — see below for why.

## The facet/group scoping question

Teun asked directly: *"Do you imagine selecting the points work for the entire plot, per
panel or per group?"* @friendly hadn't considered it. Teun's answer, which is the
authoritative design menu here:

> *"You'd swap out the `compute_group()` method for `compute_panel()` or `compute_layer()`"*

So there are **three** possible scopes, not two:

| `compute_*()` override | Facets (panels) | Groups (color/shape/facet-combo) |
|---|---|---|
| `compute_group()` | separate | separate — ggplot2's normal default for most stats |
| `compute_panel()` | separate | **pooled within each panel** — what's implemented now |
| `compute_layer()` | pooled | pooled — fully global, ignores both |

This matches what we verified empirically in `dev/test-noteworthy.R` section 7 (mtcars
faceted by `am`): facets are already correctly separated (each panel gets its own
`compute_panel()` call from ggplot2's `Stat$compute_layer()`), but `aes(color=)` groups
sharing one panel are pooled together, reproducing the *global* selection rather than a
per-group one.

Later in the thread @friendly gave the actual reasoning behind picking `compute_panel()`
over `compute_group()`:

> *"The usual ggplot-thinking approach would be to identify n unusual points within each
> group, but a stats/datavis approach would want to see just those points identified in a
> global plot."*

i.e. a deliberate statistical-graphics choice, not an oversight. Teun also noted that
per-group behavior falls out "automatically when other aesthetics are discrete" if you use
`compute_group()` — no manual group-splitting code needed, just picking the right
`compute_*()` method to override.

**Implication for `ggbiplot`**: if per-group selection (e.g. n-most-unusual per `groups=`
level) is ever wanted, it may be worth exposing the scope as a user-facing choice
(e.g. `noteworthy.scope = c("panel", "group", "layer")`) rather than picking one permanently.

## Root cause of the `label=` bug (sharper than the "doesn't work" note in stat_noteworthy.R)

@friendly's penguins example wanted labels based on Mahalanobis D² computed across all four
numeric variables, not just the two being plotted (a genuinely higher-dimensional selection
overlaid on a 2D view). Calling:

```r
stat_noteworthy(method = ids, label = ids)   # ids precomputed elsewhere, length 3
```

failed with:

```
Aesthetics must be either length 1 or the same as the data (9).
```

**Actual mechanism**: `label = ids` passed as a layer *parameter* (outside `aes()`) is
validated by ggplot2 against the data length *before* the stat runs (9 rows in that example),
not the post-filter selected-row count (3). A short externally-computed vector meant to *be*
the final labels can therefore never satisfy that check, regardless of whether it's passed to
`stat_noteworthy()` directly or via `geom_text(stat = StatNoteworthy, ...)` — both forms take
`label` through the same `params` path when passed outside `aes()`.

@EvaMaeRey's fix — what `dev/stat_noteworthy.R` now implements — is to compute `id` (row
number within the pre-filter panel data) *inside* `compute_panel()` and expose it via
`default_aes = aes(label = after_stat(id))`. Real per-point labeling then has to go through
an `aes()` mapping onto a full-length data column (resolved by ggplot2 into `data` *before*
`compute_panel()` runs, then subset alongside `x`/`y` when the stat filters to the selected
rows) — never a short post-hoc `params` vector.

This means `dev/test-noteworthy.R`'s section-1 bug check comment ("doesn't work" /
"DOESN'T WORK") could be tightened to name this mechanism explicitly rather than leaving it
as an unexplained historical flag.

## Unresolved side-thread: selecting in higher dimensions than plotted

The penguins case is a genuinely open extension problem, not solved in the thread:
selecting "noteworthy" points using a statistic computed in a space with more dimensions
than what's on the x/y axes (D² over 4 numeric variables, plotted in only 2). @EvaMaeRey
named the missing piece as something like an `aes_as_is()` — a way to carry extra data
columns through to the stat/plotting environment verbatim without repeating `aes()`
boilerplate — referencing a related pattern (`vars_pack`) from
[discussion #18](https://github.com/ggplot2-extenders/ggplot-extension-club/discussions/18#discussioncomment-10219152).
Not needed for `ggbiplot` today, but relevant if PCA-space noteworthiness (e.g. D² over all
retained PCs vs. just PC1/PC2) ever comes up.

## External input from @aphalo (ggpp author)

- `ggrepel`/`geom_text_repel()`/`geom_label_repel()` need **all** rows present in their input
  data, with unlabeled rows given `label = ""` (not dropped, not `NA` — ggplot2 drops `NA`
  rows before the geom sees them). Otherwise repulsion can't account for points it never
  sees, and repelled labels can overlap "invisible" unlabeled points.

  **This is a real gap in the current implementation**: `StatNoteworthy$compute_panel()`
  filters `data` down to only the selected rows (`data[idx, , drop = FALSE]`) before handing
  off to the geom. Any `geom = "text_repel"`/`"label_repel"` use — which
  `dev/test-noteworthy.R` section 5 already exercises — is therefore silently vulnerable to
  labels overlapping unselected points once the plot is dense enough. Not caught by the
  current tests since they don't check for overlap, only that the layer builds without error.

- Pointed at `ggpp::stat_dens2d_filter()` / `stat_dens2d_labels()`
  ([source](https://github.com/aphalo/ggpp/blob/master/R/stat-dens2d-filter.r)) as prior art
  that split "filter rows out" vs. "replace label with blank" into two separate stats,
  precisely to handle the repel-vs-non-repel distinction above. Possible model if
  `stat_noteworthy()` ever needs to support both behaviors depending on the chosen `geom`.
- Also mentioned as adjacent/competing tools (not adopted, just noted): `ggforce::geom_mark_*()`
  (group-oriented, has a selection aesthetic), `{directlabels}` (group-oriented), and `ggpp`'s
  broader nudge/position-function toolkit for label placement relative to points — relevant
  background if `varname.gap`-style label positioning (already solved elsewhere in
  `ggbiplot`) is ever generalized to `stat_noteworthy()`'s labels.

## Open naming/API question, not resolved in the thread

Whether the public function should be `stat_noteworthy()` or `geom_noteworthy()`, and
whether the default `geom` should be `"text"` or `"text_repel"` (EvaMaeRey argued for
`"text_repel"` as the default if the `ggrepel` dependency is being taken anyway). The shipped
version kept `geom = "text"` as the default and `stat_noteworthy()` as the name — worth
revisiting once/if this graduates from `dev/` into `ggbiplot()`'s public API.
