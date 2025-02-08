# How to label points selectively in ggbiplot or factoextra::fviz_pac_biplot

library(heplots)
library(dplyr)
library(ggplot2)
# library(ggbiplot)
library(factoextra)

data(peng, package="heplots")
source("C:/R/projects/Vis-MLM-book/R/penguin/penguin-colors.R")

# find potential multivariate outliers
DSQ <- heplots::Mahalanobis(peng[, 3:6])
noteworthy <- order(DSQ, decreasing = TRUE)[1:3] |> print()

peng_plot <- peng |>
  tibble::rownames_to_column(var = "id") |> 
  mutate(note = id %in% noteworthy)


peng.pca <- prcomp (~ bill_length + bill_depth + flipper_length + body_mass,
                    data=peng, scale. = TRUE)

# create vector of labels, blank except for the noteworthy
lab <- 1:nrow(peng)
lab <- ifelse(lab %in% noteworthy, lab, "")

col <- c("#F37A00, #6A3D9A, #33a02c") # pengion.colors("dark")
# options(ggplot2.discrete.fill = col,
#         ggplot2.discrete.color = col)

ggbiplot(peng.pca, # obs.scale = 1, var.scale = 1,
         choices = 3:4,
         groups = peng$species, 
         ellipse = TRUE, ellipse.alpha = 0.1,
         circle = TRUE,
         var.factor = 2.5,
         geom.ind = c("point", "text"),
         point.size = 2,
         labels = lab, labels.size = 6,
         varname.size = 5,
         clip = "off") +
  theme_minimal(base_size = 14) +
  theme_penguins("dark") +
#  theme_penguins(name = "Species") +
  theme(legend.direction = 'horizontal', legend.position = 'top') 

# first two dims
ggbiplot(peng.pca, # obs.scale = 1, var.scale = 1,
         choices = 1:2,
         groups = peng$species, 
         ellipse = TRUE, ellipse.alpha = 0.1,
         circle = TRUE,
         var.factor = 1,
         geom.ind = c("point", "text"),
         point.size = 1,
         labels = lab, labels.size = 6,
         varname.size = 5,
         clip = "off") +
  theme_minimal(base_size = 14) +
  theme_penguins("dark") +
  scale_shape_discrete() +
  theme(legend.direction = 'horizontal', legend.position = 'top') 

#------------------------------------
# try adjusting variable names to fold

vn <- rownames(peng.pca$rotation)
vn <- gsub("_", "\n", vn)
rownames(peng.pca$rotation) <- vn

ggbiplot(peng.pca, # obs.scale = 1, var.scale = 1,
         choices = 1:2,
         groups = peng$species, 
         ellipse = TRUE, ellipse.alpha = 0.1,
         circle = TRUE,
         var.factor = 1,
         geom.ind = c("point", "text"),
         point.size = 1,
         labels = lab, labels.size = 6,
         varname.size = 5,
         clip = "off") +
  theme_minimal(base_size = 14) +
  theme_penguins("dark") +
  scale_shape_discrete() +
  theme(legend.direction = 'horizontal', legend.position = 'top') 




fviz_pca_biplot(
  peng.pca,
  axes = 3:4,
  habillage = peng$species,
  addEllipses = TRUE, ellipse.level = 0.68,
  palette = peng.colors("dark"),
  arrowsize = 1.5, col.var = "black", labelsize=4,
  #  label = lab
) +
  theme(legend.position = "top")


