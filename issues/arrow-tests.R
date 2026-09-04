# Working file for comparing variable-vector arrow styles (old grid::arrow() vs. new
# ggarrow-based rendering, and different vector.args recipes), using the *crime* dataset —
# the one actually shown in README.Rmd (crime-biplot0 / crime-biplot1 chunks), not wine.
# wine was used for earlier one-off tests (issues/varname-gap.R, issues/vector-args.R) but
# isn't representative of what the README/pkgdown site actually shows.
#
# Decision so far (2026-09-04): keep ggarrow's default arrow_head_wings() ornament (Michael
# prefers its look to arrow_head_line()), but thin the default linewidth from 1.4 (matched
# the old grid::arrow() shaft width) down to 0.9, since arrow_head_wings() reads visually
# heavier than the old plain triangular head at the same linewidth. This is now
# ggvector()'s/ggbiplot()'s actual default (see R/ggvector.R), not something you need
# vector.args for — vector.args = list(linewidth = 1.4) gets the old heavier shaft back if
# ever wanted. (Considered 0.85 first, settled on 0.9.)
#
# Still open / for you to explore below: whether 0.9 is the right number, whether it should
# vary with plot size, and whether the crime-biplot1 (grouped, ellipses, var.factor = 1.4)
# case wants a different value than the plainer crime-biplot0 case.

devtools::load_all(".", quiet = TRUE)
library(ggplot2)
library(patchwork)  # only needed for the side-by-side comparisons below

# --- old (pre-ggarrow) implementation, straight from git history, for comparison ---------
# commit 3674044 is "Draw variable vectors with ggarrow instead of grid::arrow()"; its parent
# (3674044~1) has the last grid::arrow()-based ggbiplot(). Re-source it under a different name.
old_src <- system2("git", c("show", "3674044~1:R/ggbiplot.r"), stdout = TRUE)
old_src <- sub("^ggbiplot <- function", "ggbiplot_old <- function", old_src)
eval(parse(text = old_src), envir = globalenv())

data(crime)
crime.pca <- crime |>
  dplyr::select(where(is.numeric)) |>
  prcomp(scale. = TRUE) |>
  reflect()

# exact params from README.Rmd's crime-biplot1 chunk
crime_args <- list(
  pcobj = crime.pca,
  groups = crime$region,
  geom.ind = "text",  # NB: README.Rmd's crime-biplot1 chunk doesn't set this, so replaying
  # its code as-is against the *current* package shows plain colored points with no state
  # labels at all — geom.ind defaults to "point" (added v0.6.4) and, unlike some pre-0.6.4
  # behavior, passing `labels=` no longer auto-switches to text. The *committed*
  # man/figures/README-crime-biplot1-1.png (state abbreviations, no point markers) must
  # predate that default. Added geom.ind = "text" here to reproduce the actual intended
  # look; separately, README.Rmd itself should probably gain this too. Unrelated to arrows —
  # flagging since it'll bite whoever next re-knits the README.
  labels = crime$st,
  labels.size = 4,
  var.factor = 1.4,
  ellipse = TRUE, ellipse.prob = 0.5, ellipse.alpha = 0.1,  # NB: README.Rmd currently has
  # `ellipse.level = 0.5`, which isn't a ggbiplot() argument (the real one is
  # `ellipse.prob`) — silently absorbed by `...` and doing nothing there. Using the
  # presumably-intended `ellipse.prob` here; flag/fix in README.Rmd separately.
  circle = TRUE,
  varname.size = 4,
  varname.color = "black"
)

p_old <- do.call(ggbiplot_old, crime_args) +
  ggtitle("old: grid::arrow()") +
  labs(fill = "Region", color = "Region") + theme(legend.position = "none")

p_new_default <- do.call(ggbiplot, crime_args) +
  ggtitle("new default: arrow_head_wings(), linewidth = 0.9") +
  labs(fill = "Region", color = "Region") + theme(legend.position = "none")

p_new_heavy <- do.call(ggbiplot, c(crime_args, list(vector.args = list(linewidth = 1.4)))) +
  ggtitle("new: vector.args = list(linewidth = 1.4) — old shaft weight") +
  labs(fill = "Region", color = "Region") + theme(legend.position = "none")

combined <- p_old + p_new_default + p_new_heavy +
  plot_layout(nrow = 1) +
  plot_annotation(title = "crime data: variable-vector arrows, grid::arrow() vs. ggarrow",
                   subtitle = "reproduces README.Rmd's crime-biplot1 example")

out_file <- file.path(tempdir(), "arrow_tests_crime.png")
ggsave(out_file, combined, width = 15, height = 5.5, dpi = 120)
cat("saved:", out_file, "\n")

# Individual plots are also left in the environment (p_old, p_new_default, p_new_heavy) for
# interactive tweaking — e.g. print(p_new_default), or try more vector.args recipes:
#
# do.call(ggbiplot, c(crime_args, list(vector.args = list(linewidth = 0.6, length = 5))))
