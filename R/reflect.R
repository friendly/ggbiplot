
#' Reflect Columns in a Principal Component-like Object
#' 
#' Principle component-like objects have variable loadings (the eigenvectors of the covariance/correlation matrix)
#' whose signs are arbitrary, in the sense that a given column can be 
#' reflected (multiplied by -1) without changing the fit.
#' 
#' This function allows one to reflect any columns of the variable
#' loadings (and corresponding observation scores). Coordinates for quantitative
#' supplementary variables are also reflected if present.
#' This is often
#' useful for interpreting a biplot, for example when a component (often the first) has all negative signs.
#'
#' @param pcobj     an object returned by \code{\link[stats]{prcomp}}, \code{\link[stats]{princomp}}, 
#'                  \code{\link[FactoMineR]{PCA}}, or \code{\link[MASS]{lda}}
#' @param columns   a vector of indices of the columns to reflect
#'
#' @return    The pca-like object with specified columns of the 
#'            variable loadings and observation scores multiplied
#'            by -1.
#' @author Michael Friendly
#' @export
#' @seealso \code{\link[stats]{prcomp}},  \code{\link[stats]{princomp}}, 
#'          \code{\link[FactoMineR]{PCA}}, \code{\link[MASS]{lda}}
#'
#' @examples
#' data(crime)
#' crime.pca <- 
#'   crime |> 
#'   dplyr::select(where(is.numeric)) |>
#'   prcomp(scale. = TRUE)
#'   
#'  biplot(crime.pca)
#'  
#'  crime.pca <- reflect(crime.pca)  # reflect columns 1:2
#'  biplot(crime.pca)
#'  
#'  iris.lda <- MASS::lda(Species ~ ., data=iris)
#'  #reflect the first dimension
#'  iris.lda1 <- reflect(iris.lda, columns = 1)
#'  # compare predicted scores
#'  predict(iris.lda)$x |> head()
#'  predict(iris.lda1)$x |> head()

reflect <- function(pcobj, columns = 1:2){

  check <- function(x, cols){
    if(!all(cols %in% 1:ncol(x))) stop("Illegal columns selected:",
                                       paste(cols, collapse = ", "))
  }

  if(inherits(pcobj, 'prcomp')){
    check(pcobj$rotation, columns)
    pcobj$rotation[, columns] <- -1 * pcobj$rotation[, columns]
    pcobj$x[, columns]        <- -1 * pcobj$x[, columns]
  } 
  else if(inherits(pcobj, 'princomp')) {
    check(pcobj$loadings, columns)
    pcobj$loadings[, columns] <- -1 * pcobj$loadings[, columns]
    pcobj$scores[, columns]   <- -1 * pcobj$scores[, columns]
  } 
  else if(inherits(pcobj, 'PCA')) {
    check(pcobj$var$coord, columns)
    pcobj$var$coord[, columns] <- -1 * pcobj$var$coord[, columns]
    pcobj$ind$coord[, columns]   <- -1 * pcobj$ind$coord[, columns]
    # reflect quanti.sup$coord if that is present
    if ("quanti.sup" %in% names(pcobj)) pcobj$quanti.sup$coord[, columns] <- -1 * pcobj$quanti.sup$coord[, columns]
  } 
  else if(inherits(pcobj, "lda")) {
 #   lda objects don't have a scores (x) component. They come from predict()
    u <- predict(pcobj)$x
    check(u, columns)
    pcobj$scaling[, columns] <- -1 * pcobj$scaling[, columns]
  }
  else {
    stop('Expected a object of class "prcomp", "princomp", "PCA", or "lda"')
  }
  
  pcobj
  
}