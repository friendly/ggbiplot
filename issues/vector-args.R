# Visual test/demo for ggbiplot(vector.args = list(...))
#
# Background: Michael compared the new ggarrow-based arrows (default arrow_head_wings())
# against the README's previously-published grid::arrow() rendering and found the new
# default noticeably thicker/heavier-looking (TASKS-all.md, "ggarrow integration — plan",
# step 8 note), even though the shaft `linewidth` default (1.4) is unchanged from before —
# the bulk comes from the wings ornament shape, not the line. Rather than adding one
# ggbiplot()-level arg per ggarrow knob (linewidth, arrow_head, justify, sep, distort, ...),
# went with a generic passthrough: `vector.args`, a named list merged over ggvector()'s
# defaults via utils::modifyList() and forwarded with do.call(). Anything not matched by a
# ggvector() formal (e.g. `justify`) flows on through ggvector()'s own `...` to
# ggarrow::geom_arrow_segment(). Applies to the arrow layer only, not the text labels.

devtools::load_all(".", quiet = TRUE)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)

base_args <- list(obs.scale = 1, var.scale = 1, varname.size = 4,
                   groups = wine.class, ellipse = TRUE, circle = TRUE)

p_default <- do.call(ggbiplot, c(list(wine.pca), base_args))

# thinner arrows, addressing the "too thick" note
p_thin <- do.call(ggbiplot, c(list(wine.pca), base_args,
                               list(vector.args = list(linewidth = 0.5))))

# different arrowhead ornament (a plain line, not the filled wings)
p_line_head <- do.call(ggbiplot, c(list(wine.pca), base_args,
                                    list(vector.args = list(
                                      linewidth = 0.6,
                                      arrow_head = ggarrow::arrow_head_line()))))

# sanity check: a ggarrow-only param not in ggvector()'s formals (justify) still reaches
# geom_arrow_segment() via ggvector()'s own `...`
p_justify <- do.call(ggbiplot, c(list(wine.pca), base_args,
                                  list(vector.args = list(linewidth = 0.6, justify = 1))))

print(p_default)
print(p_thin)
print(p_line_head)
print(p_justify)
