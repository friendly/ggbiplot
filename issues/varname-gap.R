# Visual test/demo for ggbiplot(varname.gap = ...)
#
# Background: ggarrow::geom_arrow_segment()'s resect_head shortens the arrow from its
# tip, in millimetres (a fixed physical distance, not in data units of the PC scores).
# ggvector() exposed this as `gap`; ggbiplot() now threads it through as `varname.gap`.
# Renamed from an earlier working name `resect` (see TASKS-all.md, "ggarrow integration
# — plan", step 7) because it reads more clearly at the ggbiplot() call site.

devtools::load_all(".", quiet = TRUE)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)

p_nogap <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, varname.size = 4,
                     groups = wine.class, ellipse = TRUE, circle = TRUE)

p_gap <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, varname.size = 4,
                   groups = wine.class, ellipse = TRUE, circle = TRUE,
                   varname.gap = 3)

# Expected: in p_gap, arrowheads stop ~3mm short of their true endpoint (small visible
# gap before the label / before crossing the correlation circle), vs. touching it in
# p_nogap. The label position itself is unaffected (gap only shortens the drawn arrow).
print(p_nogap)
print(p_gap)
