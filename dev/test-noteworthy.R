# Bug check and worked examples for dev/stat_noteworthy.R (StatNoteworthy / stat_noteworthy()).
# Run this after sourcing/loading that file.

library(ggplot2)
library(heplots)   # for noteworthy()

source("dev/stat_noteworthy.R")

# ---- 1. Bug check: does `stat_noteworthy(method = ids, label = ids)` actually work? -------
# MF: Is this needed anymore??? Or, maybe keep as a way to show ggplot debugging
# 
# heplots' own draft flagged this form as "DOESN'T WORK", advising
# `geom_text(stat = StatNoteworthy, method = ids, ...)` instead. Non-confounded reproduction
# here: pick `ids` deliberately NOT equal to the selected rows' row-numbers, with a
# non-numeric label, so a silently-ignored `label` would be visibly wrong.
#
# Result (ggplot2 4.0.3): the reported bug does NOT reproduce -- both forms work identically.
# heplots' original test case just happened to pick `ids` equal to the selected points'
# actual row-numbers-within-panel, so a broken `label` (silently falling back to the default
# row-number label) looked identical to a working one. Both forms are kept
# working/documented regardless, since `geom_text(stat = StatNoteworthy, ...)` is a
# legitimate, idiomatic ggplot2 pattern either way.

set.seed(1)
bugcheck_df <- data.frame(
  x = 1:20,
  y = c(rnorm(18), 20, -20),
  nm = paste0("case_", LETTERS[1:20])
)
bugcheck_ids <- c(19, 20)
bugcheck_labels <- c("OUTLIER_HI", "OUTLIER_LO")

bugcheck_gp <- ggplot(bugcheck_df, aes(x = x, y = y)) + geom_point()

bugcheck_A <- ggplot_build(
  bugcheck_gp + stat_noteworthy(method = bugcheck_ids, label = bugcheck_labels)
)$data[[2]][, c("x", "y", "id", "label")]

bugcheck_B <- ggplot_build(
  bugcheck_gp + geom_text(stat = StatNoteworthy, method = bugcheck_ids, label = bugcheck_labels)
)$data[[2]][, c("x", "y", "id", "label")]

stopifnot(identical(bugcheck_A$label, bugcheck_labels))
stopifnot(identical(bugcheck_B$label, bugcheck_labels))
cat("Bug check passed: both stat_noteworthy(label=) and geom_text(stat=StatNoteworthy, label=)",
    "correctly set constant labels under ggplot2", as.character(packageVersion("ggplot2")), "\n")

# ---- 2. Basic examples ---------------------------------------------------------------------

data(mtcars)
gp_mtcars <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

# 2a. Default: method = "mahal", row-number labels
p_basic <- gp_mtcars + stat_noteworthy(color = "red") +
  labs(title = "method = 'mahal', row-number labels")
p_basic

# TODO: MF: row labels default are centered on the points, NOT GOOD. A reasonable default would be to out-justify the -- to the left of points with x < mean, otherwise to the right. In base R, this is usually handled by a `pos` argument, calculated from the data

# 2b. User-mapped meaningful labels (car model name) instead of row number. The `label`
# mapping is scoped to stat_noteworthy()'s own layer (not the base ggplot() call) so
# geom_smooth() doesn't inherit -- and warn about -- an aesthetic it has no use for.
p_named <- gp_mtcars +
  stat_noteworthy(aes(label = rownames(mtcars)), color = "red", vjust = -0.6) +
  labs(title = "method = 'mahal', user-mapped labels (car model name)")
p_named

# ---- 3. Each `method` type documented by heplots::noteworthy() ---------------------------

mod <- lm(mpg ~ wt, data = mtcars)

p_dsq <- gp_mtcars + stat_noteworthy(method = "dsq", color = "blue") +
  labs(title = "method = 'dsq' (squared Euclidean distance from centroid)")
p_dsq

p_x   <- gp_mtcars + stat_noteworthy(method = "x", color = "blue") +
  labs(title = "method = 'x' (deviation from mean of x)")
p_y   <- gp_mtcars + stat_noteworthy(method = "y", color = "blue") +
  labs(title = "method = 'y' (deviation from mean of y)")
p_r   <- gp_mtcars + stat_noteworthy(method = "r", color = "blue") +
  labs(title = "method = 'r' (|y|)")
p_ry  <- gp_mtcars + stat_noteworthy(method = "ry", color = "blue") +
  labs(title = "method = 'ry' (residual from y ~ x)")
p_caseids <- gp_mtcars + stat_noteworthy(method = c(1, 15, 20), color = "darkgreen") +
  labs(title = "method = case IDs c(1, 15, 20)")
p_caseids

p_cooksd  <- gp_mtcars + stat_noteworthy(method = cooks.distance(mod), n = 3, color = "purple") +
  labs(title = "method = cooks.distance(mod), n = 3")
p_cooksd

# ---- 4. `level` filtering ------------------------------------------------------------------

# only label points whose Mahalanobis D^2 exceeds the 0.99 chi-square quantile (fewer than n)
p_level <- gp_mtcars + stat_noteworthy(method = "mahal", n = 10, level = 0.99, color = "red") +
  labs(title = "method = 'mahal', n = 10, level = 0.99")

# ---- 5. Multiple geoms -----------------------------------------------------------------

p_geom_text  <- gp_mtcars + stat_noteworthy(geom = "text", color = "red") +
  labs(title = "geom = 'text'")
p_geom_label <- gp_mtcars + stat_noteworthy(geom = "label", color = "red") +
  labs(title = "geom = 'label'")

if (requireNamespace("ggrepel", quietly = TRUE)) {
  library(ggrepel)   # geom string lookup needs the package attached, not just loaded
  p_geom_repel <- gp_mtcars + stat_noteworthy(geom = "text_repel", color = "red") +
    labs(title = "geom = 'text_repel' (ggrepel)")
} else {
  p_geom_repel <- NULL
}

# ---- 6. Compositional sanity check: on top of a real ggbiplot() -----------------------
# Context only -- confirms the layer composes cleanly with ggbiplot()'s own point/ellipse/
# vector layers; does NOT add any new ggbiplot() argument.

devtools::load_all(".", quiet = TRUE)

data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
# stat_noteworthy() inherits ggbiplot()'s own aes(x = xvar, y = yvar) mapping and df.u data --
# no need to pass data/mapping explicitly, same as adding any other ggplot2 layer with `+`.
p_biplot <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
                     groups = wine.class, ellipse = TRUE, circle = TRUE) +
  stat_noteworthy(method = "mahal", n = 5, color = "black") +
  labs(title = "stat_noteworthy() composed with ggbiplot() (wine data)")
p_biplot

# ---- 7. Faceting and grouping: is selection scoped per-panel / per-group? -----------------
# TASKS-all.md flagged this as an open question. StatNoteworthy overrides compute_panel()
# (not compute_group()), and ggplot2's Stat$compute_layer() splits data by PANEL *before*
# calling compute_panel() -- but does NOT split further by `group` unless compute_panel()
# itself delegates to compute_group(). Prediction:
#   - facet_wrap()/facet_grid() panels -> each panel is a separate compute_panel() call ->
#     selection SHOULD already be correctly scoped to each facet's own points.
#   - aes(color = ...)/aes(group = ...) WITHOUT faceting -> all groups share one panel -> one
#     compute_panel() call sees every group's data pooled together -> selection is GLOBAL
#     across groups, not per-group.
# Tested with mtcars, faceted by am (0 = automatic, 19 cars; 1 = manual, 13 cars).

mtcars$rowname <- rownames(mtcars)

# 7a. Ground truth: noteworthy() computed separately within each am subset -- this is what
# "correct per-facet selection" should match.
truth <- lapply(split(mtcars, mtcars$am), function(d) {
  idx <- heplots::noteworthy(x = d$wt, y = d$mpg, n = 3, method = "mahal")
  d$rowname[idx]
})
names(truth) <- paste0("am=", names(truth))

# For contrast: what a GLOBAL (unfaceted) selection over all 32 cars picks
global_idx <- heplots::noteworthy(x = mtcars$wt, y = mtcars$mpg, n = 3, method = "mahal")

# 7b. Faceted plot: does stat_noteworthy() reproduce the per-facet ground truth?
p_facet_am <- gp_mtcars +
  facet_wrap(~am, labeller = label_both) +
  stat_noteworthy(aes(label = rownames(mtcars)), method = "mahal", n = 3, color = "red") +
  labs(title = "facet_wrap(~am): selection scoped per facet")
p_facet_am

# p_facet_am's layers are geom_point(), geom_smooth() (from gp_mtcars), then stat_noteworthy()
built_facet_am <- ggplot_build(p_facet_am)$data[[3]]
by_panel <- split(built_facet_am$label, built_facet_am$PANEL)
names(by_panel) <- paste0("am=", levels(factor(mtcars$am))[as.integer(names(by_panel))])

for (nm in names(truth)) {
  matched <- setequal(truth[[nm]], by_panel[[nm]])
  cat(sprintf("%s: ground truth = {%s}; stat_noteworthy facet = {%s}; MATCH = %s\n",
              nm, paste(sort(truth[[nm]]), collapse = ", "),
              paste(sort(by_panel[[nm]]), collapse = ", "), matched))
}
# Result: MATCH = TRUE for both panels -- facet_wrap() is already correctly scoped, no fix
# needed there.

# 7c. Contrast: aes(color =) grouping WITHOUT faceting -- expected to NOT be per-group, since
# compute_panel() (not compute_group()) pools all groups sharing one panel together.
p_grouped_am <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(am))) +
  geom_point() +
  stat_noteworthy(aes(label = rownames(mtcars)), method = "mahal", n = 3, color = "black") +
  labs(title = "aes(color=) grouping WITHOUT faceting: selection pools groups (known gap)")
p_grouped_am

grouped_labels <- sort(ggplot_build(p_grouped_am)$data[[2]]$label)
cat("aes(color=) selection:", paste(grouped_labels, collapse = ", "), "\n")
cat("global (unfaceted) ground truth:", paste(sort(mtcars$rowname[global_idx]), collapse = ", "), "\n")
cat("MATCH to global (i.e. groups pooled, NOT per-group) =",
    setequal(grouped_labels, mtcars$rowname[global_idx]), "\n")
# Result: MATCH = TRUE -- confirms the documented gap: color-mapped groups sharing one panel
# are NOT separated by compute_panel(). Fixing this would mean switching to compute_group()
# (or having compute_panel() split on data$group itself) -- relevant since ggbiplot()'s own
# `groups=` argument maps to exactly this pattern (color, one panel).

# ---- 8. Save all examples for visual review ------------------------------------------------

outdir <- file.path(tempdir(), "noteworthy")
dir.create(outdir, showWarnings = FALSE)

save_one <- function(p, name) if (!is.null(p)) ggsave(file.path(outdir, paste0(name, ".png")), p, width = 6, height = 5)

save_one(p_basic, "01_basic_mahal")
save_one(p_named, "02_named_labels")
save_one(p_dsq, "03_method_dsq")
save_one(p_x, "04_method_x")
save_one(p_y, "05_method_y")
save_one(p_r, "06_method_r")
save_one(p_ry, "07_method_ry")
save_one(p_caseids, "08_method_caseids")
save_one(p_cooksd, "09_method_cooksd")
save_one(p_level, "10_level_filter")
save_one(p_geom_text, "11_geom_text")
save_one(p_geom_label, "12_geom_label")
save_one(p_geom_repel, "13_geom_repel")
save_one(p_biplot, "14_ggbiplot_composition")

cat("Done. Examples saved to", outdir, "\n")
