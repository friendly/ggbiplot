# Selectively label "noteworthy" (unusual) points in a ggplot2 scatterplot, via a
# StatNoteworthy ggproto + stat_noteworthy() constructor.
#
# Adapted from heplots' unfinished/unexported draft (heplots/dev/stat_noteworthy.R,
# heplots/dev/noteworthy-notes.md), which built the ggproto around the already-shipped
# `heplots::noteworthy(x, y, n, method, level, ...)` (heplots 1.7.4) -- a general point-
# selection utility extending `car::showLabels()`'s `method` options ("mahal", "dsq", "x",
# "y", "r", "ry", or a supplied numeric/case-ID vector). That draft's own commented-out
# examples flagged `stat_noteworthy(method = ids, label = ids)` as "DOESN'T WORK", advising
# `geom_text(stat = StatNoteworthy, method = ids, ...)` instead -- a form independently
# confirmed by Evangeline Reynolds' exploration in Vis-MLM-book/test/Gina-mahalanobis.Rmd.
# See dev/test-noteworthy.R for a non-confounded reproduction showing that reported bug does
# NOT actually reproduce under current ggplot2 -- both forms work identically.
#
# Scope: this file prototypes the labeling MECHANISM only. It does not wire any argument into
# ggbiplot()/ggvector() -- see TASKS-all.md's "noteworthy" entry for that follow-up decision.

#' Selectively label noteworthy (unusual) points in a 2D scatterplot
#'
#' @description
#' An important feature of many statistical plots is the ability to selectively label
#' "unusual" or "noteworthy" observations, such as those with large residuals, high leverage,
#' or those farthest from the centroid of a scatterplot. `stat_noteworthy()` selects such
#' points using [heplots::noteworthy()], with a choice of `geom` to draw the labels (e.g.
#' `"text"`, `"label"`, or, if `ggrepel` is installed, `"text_repel"`/`"label_repel"`).
#'
#' In simple cases, labels can be added to a ggplot object using `stat_noteworthy()` directly.
#' Alternatively, use `geom_text()`, `geom_label()`, ... and specify `stat = StatNoteworthy` to
#' select the points to be labeled with that geom instead.
#'
#' @details
#' The `method` argument determines how the labeled points are selected -- see
#' [heplots::noteworthy()] for the full set of options (Mahalanobis/Euclidean distance from
#' the centroid, absolute deviation from the mean of `x`/`y`, absolute residual from a
#' `y ~ x` fit, a vector of case IDs, or a supplied numeric vector such as
#' `cooks.distance(mod)`).
#'
#' Labels default to the row number within the plot's data (`after_stat(id)`); map your own
#' `aes(label = ...)` (e.g. a real ID or name column) to use meaningful labels instead -- that
#' mapping takes precedence over the default.
#'
#' @param mapping,data,position,na.rm,show.legend,inherit.aes standard layer arguments
#' @param geom which geom draws the label: `"text"`, `"label"`, or (if `ggrepel` is installed)
#'   `"text_repel"`/`"label_repel"`
#' @param method,n,level passed to [heplots::noteworthy()] -- see its documentation for the
#'   full set of `method` options
#' @param ... other arguments passed to the underlying geom (e.g. `size`, `color`)
#' @return A ggplot2 layer that can be added to an existing plot with `+`.
#' @export
#'
#' @examples
#' # see dev/test-noteworthy.R for worked examples covering every `method` option, multiple
#' # geoms, `level` filtering, and composition with ggbiplot()

#' @rdname stat_noteworthy
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
StatNoteworthy <- ggplot2::ggproto(
  "StatNoteworthy", ggplot2::Stat,

  required_aes = c("x", "y"),

  # compute_panel (not compute_group): selects globally within a panel, regardless of any
  # groups/color mapping. Selecting *within* each group separately (e.g., the n most unusual
  # points per species) would need compute_group instead -- a reasonable future option, not
  # built here to keep this prototype to the essential mechanism.
  compute_panel = function(data, scales, method = "mahal", n = 5, level = NULL) {
    data$id <- seq_len(nrow(data))
    idx <- heplots::noteworthy(x = data$x, y = data$y, n = n, method = method, level = level)
    data[idx, , drop = FALSE]
  },

  # Falls back to the row number (within the full, pre-selection panel data) when the user
  # doesn't map their own `label` aesthetic. A real aes(label = <id column>) mapping at the
  # ggplot()/layer level takes precedence over this default, since ggplot2 resolves aes
  # mappings into `data` columns before compute_panel() ever runs.
  default_aes = ggplot2::aes(label = ggplot2::after_stat(id))
)

#' @rdname stat_noteworthy
#' @export
stat_noteworthy <- function(
    mapping = NULL, data = NULL, geom = "text", position = "identity",
    ..., method = "mahal", n = 5, level = NULL,
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatNoteworthy, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      n = n,
      level = level,
      na.rm = na.rm,
      ...
    )
  )
}
