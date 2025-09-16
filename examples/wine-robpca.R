library(rospca)
library(ggbiplot)
data(wine, package = "ggbiplot")

wine.pca <- prcomp(wine, scale=TRUE)

wine.pca <- reflect(wine.pca)


ggbiplot(wine.pca, 
         obs.scale = 1, var.scale = 1,
         groups = wine.class, 
         varname.size = 4,
         ellipse = TRUE, 
         circle = TRUE) +
  labs(fill = "Cultivar", color = "Cultivar") +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# direct labels 
means <- aggregate(cbind(PC1, PC2) ~ wine.class,
    data = wine.pca$x, FUN = mean)

ggbiplot(wine.pca, 
         obs.scale = 1, var.scale = 1,
         groups = wine.class, 
         varname.size = 4,
         ellipse = TRUE, 
         circle = TRUE) +
  geom_label(data = means, aes(x=PC1, y=PC2, label = wine.class)) +
  theme(legend.position = 'none')


wine.rpca <- robpca(wine, k = 2, scale=TRUE)
names(wine.rpca)

# [1] "loadings"    "eigenvalues" "scores"      "center"      "k"           "H0"          "H1"          "alpha"       "h"          
# [10] "sd"          "od"          "cutoff.sd"   "cutoff.od"   "flag.sd"     "flag.od"     "flag.all" 

class(wine.rpca)
# list

col <- c("red", "green", "blue")[wine.class]
diagPlot(wine.rpca, col = col)

wine.rpca$loadings


#ggbiplot(wine.rpca)



