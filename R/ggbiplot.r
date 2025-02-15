# 
#  ggbiplot.r
#  
#  Copyright 2011 Vincent Q. Vu.
# 

#' Biplot for Principal Components using ggplot2
#' 
#' @description 
#' A biplot simultaneously displays information on the observations (as points)
#' and the variables (as vectors) in a multidimensional dataset. The 2D biplot
#' is typically based on the first two principal components of a dataset, giving a rank 2 approximation 
#' to the data. The “bi” in biplot refers to the fact that two sets of points (i.e., the rows and
#' columns of the data matrix) are visualized by scalar products, not the fact
#' that the display is usually two-dimensional.
#' 
#' The biplot method for principal component analysis was originally defined by Gabriel (1971, 1981).
#' Gower & Hand (1996) give a more complete treatment. Greenacre (2010) is a practical user-oriented guide to biplots.
#' Gower et al. (2011) is the most up to date
#' exposition of biplot methodology.
#' 
#' This implementation handles the results of a principal components analysis using 
#' \code{\link[stats]{prcomp}}, \code{\link[stats]{princomp}}, \code{\link[FactoMineR]{PCA}} and \code{\link[ade4]{dudi.pca}};
#' also handles a discriminant analysis using \code{\link[MASS]{lda}}.
#' 
#' @details
#' The biplot is constructed by using the singular value decomposition (SVD) to obtain a low-rank 
#' approximation to the data matrix \eqn{\mathbf{X}_{n \times p}} (centered, and optionally scaled to unit variances)
#' whose \eqn{n} rows are the observations 
#' and whose \eqn{p} columns are the variables. 
#' 
#' Using the SVD, the matrix \eqn{\mathbf{X}}, of rank \eqn{r \le p}
#' can be expressed \emph{exactly} as
#' \deqn{\mathbf{X} = \mathbf{U} \mathbf{\Lambda} \mathbf{V}'
#'                  = \Sigma_i^r \lambda_i \mathbf{u}_i \mathbf{v}_i' \; ,}
#' 
#' where 
#' \itemize{
#'    \item \eqn{\mathbf{U}} is an \eqn{n \times r} orthonormal matrix of observation scores; these are also the eigenvectors
#'          of \eqn{\mathbf{X} \mathbf{X}'},
#'    \item \eqn{\mathbf{\Lambda}} is an \eqn{r \times r} diagonal matrix of singular values, 
#'          \eqn{\lambda_1 \ge \lambda_2 \ge \cdots \lambda_r} 
#'         % which are also the square roots
#'         % of the eigenvalues of \eqn{\mathbf{X} \mathbf{X}'}. 
#'    \item \eqn{\mathbf{V}} is an \eqn{r \times p} orthonormal matrix of variable weights and also the eigenvectors
#'          of \eqn{\mathbf{X}' \mathbf{X}}.
#' }
#' 
#' Then, a rank 2 (or 3) PCA approximation \eqn{\widehat{\mathbf{X}}} to the data matrix used in the biplot
#' can be obtained from the first 2 (or 3)
#' singular values \eqn{\lambda_i}
#' and the corresponding \eqn{\mathbf{u}_i, \mathbf{v}_i} as
#' 
#' \deqn{\mathbf{X} \approx \widehat{\mathbf{X}} = \lambda_1 \mathbf{u}_1 \mathbf{v}_1' + \lambda_2 \mathbf{u}_2 \mathbf{v}_2' \; .}
#'
#' The variance of \eqn{\mathbf{X}} accounted for by each term is \eqn{\lambda_i^2}.
#' 
#' The biplot is then obtained by overlaying two scatterplots that share a common set of axes and have a between-set scalar 
#' product interpretation. Typically, the observations (rows of \eqn{\mathbf{X}}) are represented as points
#' and the variables (columns of \eqn{\mathbf{X}}) are represented as vectors from the origin.
#' 
#' The \code{scale} factor, \eqn{\alpha} allows the variances of the components to be apportioned between the
#' row points and column vectors, with different interpretations, by representing the approximation
#' \eqn{\widehat{\mathbf{X}}} as the product of two matrices,
#' 
#' \deqn{\widehat{\mathbf{X}} = (\mathbf{U} \mathbf{\Lambda}^\alpha) (\mathbf{\Lambda}^{1-\alpha} \mathbf{V}')}
#' 
#' The choice \eqn{\alpha = 1}, assigning the singular values totally to the left factor,
#'  gives a distance interpretation to the row display and 
#' \eqn{\alpha = 0} gives a distance interpretation to the column display.
#' \eqn{\alpha = 1/2} gives a symmetrically scaled biplot.
#' 
#' When the singular values are assigned totally to the left or to the right factor, the resultant 
#' coordinates are called \emph{principal coordinates} and the sum of squared coordinates
#' on each dimension equal the corresponding singular value.
#' The other matrix, to which no part of the singular 
#' values is assigned, contains the so-called \emph{standard coordinates} and have sum of squared
#' values equal to 1.0. 
#' 
#' \bold{Scales and legend}
#' 
#' When the `groups` argument is not \code{NULL}, the function uses that value to set the aesthetics for `color`, `fill` and `shape`.
#' If you override the defaults using \code{scale_color_discrete}, etc., you may find that duplicate legends are produced.
#' To avoid this, you need to have the same name for aesthetics to be merged in the legend, for example,
#' \preformatted{
#' scale_fill_discrete(name = 'Species') 
#' scale_color_discrete(name = 'Species')
#' }
#' or,
#' \preformatted{
#' labs(fill = "Species", color = "Species")
#' }
#' 
#' 
#' @param pcobj           an object returned by \code{\link[stats]{prcomp}}, \code{\link[stats]{princomp}}, 
#'                        \code{\link[FactoMineR]{PCA}}, \code{\link[ade4]{dudi.pca}}, or \code{\link[MASS]{lda}}
#' @param choices         Which components to plot? An integer vector of length 2.
#' @param scale           Covariance biplot (\code{scale = 1}), form biplot (\code{scale = 0}). 
#'                        When \code{scale = 1} (the default), the inner product 
#'                        between the variables approximates the covariance and the distance between the points 
#'                        approximates the Mahalanobis distance.
#' @param obs.scale       Scale factor to apply to observations
#' @param var.scale       Scale factor to apply to variables
#' @param var.factor      Factor to be applied to variable vectors after scaling. This allows the variable vectors to be reflected
#'                        (\code{var.factor = -1}) or expanded in length (\code{var.factor > 1}) for greater visibility.
#'                        \code{\link{reflect}} provides a simpler way to reflect the variables.
#' @param pc.biplot       Logical, for compatibility with \code{biplot.princomp()}. If \code{TRUE}, use what Gabriel (1971) 
#'                        refers to as a "principal component biplot", with \eqn{\alpha = 1} and observations scaled 
#'                        up by \eqn{sqrt(n)} and variables scaled down by \eqn{sqrt(n)}. Then inner products between 
#'                        variables approximate covariances and distances between observations approximate 
#'                        Mahalanobis distance.
#' @param groups          Optional factor variable indicating the groups that the observations belong to. 
#'                        If provided the points will be colored according to groups and this allows data ellipses also
#'                        to be drawn when \code{ellipse = TRUE}.
#' @param geom.ind        a text specifying the geometry to be used for the observations. Allowed 
#'                        values are among \code{c("point", "text")}. Use 
#'                        \code{"point"} (to show only points); \code{"text"} to show only labels; 
#'                        \code{c("point", "text")}  to show both. 
#' @param geom.var        a text specifying the geometry to be used for the variables. Allowed 
#'                        values are among \code{c("arrow", "text")}. Use 
#'                        \code{"arrow"} (to show only vectors); \code{"text"} to show only labels; 
#'                        \code{c("arrow", "text")}  to show both. 
#' @param point.size      Size of observation points.
#' @param ellipse         Logical; draw a normal data ellipse for each group?
#' @param ellipse.prob    Coverage size of the data ellipse in Normal probability
#' @param ellipse.linewidth    Thickness of the line outlining the ellipses
#' @param ellipse.fill    Logical; should the ellipses be filled?
#' @param ellipse.alpha   Transparency value (0 - 1) for filled ellipses
#' @param labels          Optional vector of labels for the observations. Often, this will be specified as the \code{row.names()}
#'                        of the dataset.
#' @param labels.size     Size of the text used for the point labels
#' @param alpha           Alpha transparency value for the points (0 = transparent, 1 = opaque)
#' @param circle          draw a correlation circle? (only applies when prcomp was called with 
#'                        \code{scale = TRUE} and when \code{var.scale = 1})
#' @param circle.prob     Size of the correlation circle
#' @param var.axes        logical; draw arrows for the variables?
#' @param varname.size    Size of the text for variable names
#' @param varname.color   Color for the variable vectors and names
#' @param varname.adjust  Adjustment factor the placement of the variable names, >= 1 means farther from the arrow
#' @param varname.abbrev  logical; whether or not to abbreviate the variable names, using \code{\link{abbreviate}}.
#' @param axis.title      character; the prefix used as the axis labels. Default: \code{"PC"}.
#' @param clip            should geoms be clipped at the axis limits? Default: "on"
#' @param ...             other arguments passed down
#'
#' @import     ggplot2
#' @importFrom stats predict qchisq var
#' @importFrom scales muted
## @importFrom dplyr filter n summarize select group_by
## @importFrom tidyr unnest
## @importFrom purrr map
#' 
#' @seealso 
#'   \code{\link{reflect}}, \code{\link{ggscreeplot}};
#'   \code{\link[stats]{biplot}} for the original stats package version;
#'   \code{\link[factoextra]{fviz_pca_biplot}} for the factoextra package version.
#' 
#' @author Vincent Q. Vu., Michael Friendly
#' @references 
#'   Gabriel, K. R. (1971). The biplot graphical display of matrices with application to principal component analysis. 
#'   \emph{Biometrika}, \bold{58}, 453–467. \doi{10.2307/2334381}.
#'   
#'   Gabriel, K. R. (1981). Biplot display of multivariate matrices for inspection of data and diagnosis. 
#'   In V. Barnett (Ed.), \emph{Interpreting Multivariate Data}. London: Wiley. 
#'   
#'   Greenacre, M. (2010). \emph{Biplots in Practice}. BBVA Foundation, Bilbao, Spain. 
#'   Available for free at \url{https://www.fbbva.es/microsite/multivariate-statistics/}.
#'   
#'   J.C. Gower and D. J. Hand (1996). \emph{Biplots}. Chapman & Hall.
#'   
#'   Gower, J. C., Lubbe, S. G., & Roux, N. J. L. (2011). \emph{Understanding Biplots}. Wiley.
#'   
#' @return                a ggplot2 plot object of class \code{c("gg", "ggplot")}
#' @export
#' @examples
#' data(wine)
#' library(ggplot2)
#' wine.pca <- prcomp(wine, scale. = TRUE)
#' ggbiplot(wine.pca, 
#'          obs.scale = 1, var.scale = 1, 
#'          varname.size = 4,
#'          groups = wine.class, 
#'          ellipse = TRUE, circle = TRUE)
#'
#' data(iris)
#' iris.pca <- prcomp (~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'                     data=iris,
#'                     scale. = TRUE)
#' ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
#'          groups = iris$Species, point.size=2,
#'          varname.size = 5, 
#'          varname.color = "black",
#'          varname.adjust = 1.2,
#'          ellipse = TRUE, 
#'          circle = TRUE) +
#'   labs(fill = "Species", color = "Species") +
#'   theme_minimal(base_size = 14) +
#'   theme(legend.direction = 'horizontal', legend.position = 'top')

ggbiplot <- function(pcobj, 
                     choices = 1:2, 
                     scale = 1, 
                     pc.biplot = TRUE, 
                     obs.scale = 1 - scale, 
                     var.scale = scale, 
                     var.factor = 1,    # MF
                     groups = NULL, 
                     geom.ind = "point",
                     geom.var = c("arrow", "text"),
                     point.size = 1.5,
                     ellipse = FALSE, 
                     ellipse.prob = 0.68, 
                     ellipse.linewidth = 1.3,
                     ellipse.fill = TRUE,
                     ellipse.alpha = 0.25,
                     labels = NULL, 
                     labels.size = 3, 
                     alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, 
                     circle.prob = 0.68, 
                     varname.size = 3, 
                     varname.adjust = 1.25, 
                     varname.color = "black",
                     varname.abbrev = FALSE,
                     axis.title = "PC",
                     clip = "on",
                     ...)
{

  if(length(choices) > 2) {
    warning("choices = ", choices, " of length", length(choices), " not of length 2. Only the first 2 will be used")
    choices <- choices[1:2]
  }

  # Recover the SVD
#  if(inherits(pcobj, 'prcomp')){
#     nobs.factor <- sqrt(nrow(pcobj$x) - 1)
#     d <- pcobj$sdev
#     u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
#     v <- pcobj$rotation
#   } else if(inherits(pcobj, 'princomp')) {
#     nobs.factor <- sqrt(pcobj$n.obs)
#     d <- pcobj$sdev
#     u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
#     v <- pcobj$loadings
#   } else if(inherits(pcobj, 'PCA')) {
#     nobs.factor <- sqrt(nrow(pcobj$call$X))
#     d <- unlist(sqrt(pcobj$eig)[1])
#     u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
#     v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]), FUN="/")
#   } else if(inherits(pcobj, "lda")) {
#       nobs.factor <- sqrt(pcobj$N)
#       d <- pcobj$svd
#       u <- predict(pcobj)$x/nobs.factor
#       v <- pcobj$scaling
# #      d.total <- sum(d^2)
#   } else if(inherits(pcobj, 'pca') & inherits(pcobj, 'dudi')){
#       nobs.factor <- sqrt(nrow(pcobj$tab))
#       d <- sqrt(pcobj$eig)
#       u <- pcobj$li
#       v <- pcobj$co
#   }
#   else {
#     stop('Expected a object of class "prcomp", "princomp", "PCA", c("pca", "dudi") or "lda"')
#   }
  
  svd <- get_SVD(pcobj)
  n <- svd$n
  d <- svd$D
  u <- svd$U
  v <- svd$V
  nobs.factor <- ifelse (inherits(pcobj, 'prcomp'), sqrt(n-1), sqrt(n))

  # shutup 'no visible binding...'
#  utils::globalVariables(c("xvar", "yvar", "varname", "angle", "hjust"))
  angle <- circle_chol <- ed <- hjust <- mu <- sigma <- varname <- xvar <- yvar <-NULL

  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  df.v <- var.factor * df.v

  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)

  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }

  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)

  # Scale the variable directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))

  # Change the title labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste0('standardized ', axis.title, choices)
  } else {
    u.axis.labs <- paste0(axis.title, choices)
  }

  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%%)', 
                               100 * d[choices]^2 / sum(d^2)))

  # Score labels for the observations
  if(!is.null(labels)) {
    df.u$labels <- labels
  }

  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }

  # Variable Names
  vn <- rownames(v)
  if(is.logical(varname.abbrev) & isTRUE(varname.abbrev)) {
    vn <- abbreviate(vn)
  }
  df.v$varname <- vn

  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)

  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
          xlab(u.axis.labs[1]) + 
          ylab(u.axis.labs[2]) + 
          coord_equal(clip = clip)

  # Draw either labels or points
  # if(!is.null(df.u$labels)) {
  #   if(!is.null(df.u$groups)) {
  #     g <- g + geom_text(aes(label = labels, color = groups), 
  #                        size = labels.size)
  #   } else {
  #     g <- g + geom_text(aes(label = labels), size = labels.size)      
  #   }
  # } else {
  #   if(!is.null(df.u$groups)) {
  #     g <- g + geom_point(aes(color = groups), alpha = alpha, size = point.size)
  #   } else {
  #     g <- g + geom_point(alpha = alpha, size = point.size)      
  #   }
  # }
  
  # Allow to use points and/or labels
  if("point" %in% geom.ind) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups, shape = groups), 
                          alpha = alpha, size = point.size)
    } else {
      g <- g + geom_point(alpha = alpha, size = point.size)      
    }
  }
  if("text" %in% geom.ind) {
    if(!is.null(df.u$groups)) {
      # g <- g + geom_text(aes(label = labels, color = groups),
      #                    size = labels.size, vjust = -0.5)
      g <- g + geom_text(aes(label = labels),
                         size = labels.size, vjust = -0.5)
    } else {
      g <- g + geom_text(aes(label = labels), 
                         size = labels.size, vjust = -0.5)      
    }
  }
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, 
                         color = scales::muted('white'), 
                         linewidth = 1/2, alpha = 1/3)
    }

    # Draw directions
    if("arrow" %in% geom.var) {
    arrow_style <- arrow(length = unit(1/2, 'picas'), type="closed", angle=15) 
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow_style, 
                   color = varname.color,
                   linewidth = 1.4)    # MR: was 1.2
    }
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))



    # Overlay a concentration ellipse if there are groups
      geom <- if(isTRUE(ellipse.fill)) "polygon" else "path"
      if (isTRUE(ellipse.fill)) {
      g <- g + stat_ellipse(geom="polygon",
                            aes(group = groups, 
                                color = groups,
                                fill = groups),
                            alpha = ellipse.alpha,
                            linewidth = ellipse.linewidth,
                            type = "norm", level = ellipse.prob)
      }
      else {
        g <- g + stat_ellipse(geom="path",
                              aes(group = groups, 
                                  color = groups),
                              linewidth = ellipse.linewidth,
                              type = "norm", level = ellipse.prob)
      }

  }

  # Label the variable axes
  if(var.axes & "text" %in% geom.var) {
    g <- g + 
    geom_text(data = df.v, 
              aes(label = varname, x = xvar, y = yvar, 
                  angle = angle, hjust = hjust), 
              color = varname.color, size = varname.size, lineheight = 0.75)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }

  # TODO: Add a second set of axes

  return(g)
}
