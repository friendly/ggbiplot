
#' Reflect Columns in a Principal Component-like Object
#'
#' @description
#' Principal component-like objects have variable loadings (the eigenvectors of the
#' covariance/correlation matrix) whose signs are arbitrary, in the sense that a given
#' column can be reflected (multiplied by -1) without changing the fit. `reflect()` is a
#' generic function, with methods for PCA-like objects (`prcomp`, `princomp`,
#' [FactoMineR::PCA()], `lda`) that reflect the variable loadings and corresponding
#' observation scores (and supplementary variable coordinates, for `PCA`). 
#' The S3 methods also allow for
#' a plain `data.frame`, `matrix`, or a `list` of these, that negate the specified columns
#' directly.
#'
#' This is often useful for interpreting a biplot, for example when a component (often the
#' first) has all negative signs.
#'
#' @param pcobj     an object to reflect: one of `prcomp`, `princomp`, [FactoMineR::PCA()],
#'                  `lda`, a `data.frame`, a `matrix`, or a `list` of data frames/matrices
#'                  with matching columns
#' @param columns   for PCA-like objects, a vector of indices of the dimensions to reflect
#'                  (default `1:2`). For the `data.frame`/`matrix`/`list` methods, a vector
#'                  of column indices or names to negate -- there is no sensible default
#'                  for an arbitrary table, so this must be supplied.
#' @param scale     a constant applied along with the reflection, so the selected columns
#'                  become `-scale * columns` rather than just `-1 * columns`. Default `1`
#'                  (plain reflection, no change in magnitude). Useful, e.g., to stretch or
#'                  shrink variable vectors in a biplot at the same time as reflecting them;
#'                  a negative `scale` scales without reflecting.
#' @param ...       arguments passed to methods
#'
#' @return    The object, of the same class, with the specified columns transformed to
#'            `-scale * columns` -- the variable loadings and observation scores for
#'            PCA-like objects, or the specified columns directly for a
#'            `data.frame`/`matrix`/`list`.
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
#'
#'  # reflect a column of an ordinary data.frame, by name or index
#'  mtcars2 <- reflect(mtcars, columns = "mpg")
#'  head(mtcars2$mpg)
#'
#'  # reflect AND stretch the variable vectors by a factor of 2
#'  crime.pca3 <- reflect(crime.pca, scale = 2)
reflect <- function(pcobj, ...) {
  UseMethod("reflect")
}

# shared column-validity check for all methods below
check_reflect_columns <- function(x, columns) {
  if (is.character(columns)) {
    if (!all(columns %in% colnames(x))) {
      stop("Illegal columns selected: ", paste(columns, collapse = ", "))
    }
  } else if (!all(columns %in% seq_len(ncol(x)))) {
    stop("Illegal columns selected: ", paste(columns, collapse = ", "))
  }
}

#' @rdname reflect
#' @export
reflect.prcomp <- function(pcobj, columns = 1:2, scale = 1, ...) {
  check_reflect_columns(pcobj$rotation, columns)
  pcobj$rotation[, columns] <- -scale * pcobj$rotation[, columns]
  pcobj$x[, columns]        <- -scale * pcobj$x[, columns]
  pcobj
}

#' @rdname reflect
#' @export
reflect.princomp <- function(pcobj, columns = 1:2, scale = 1, ...) {
  check_reflect_columns(pcobj$loadings, columns)
  pcobj$loadings[, columns] <- -scale * pcobj$loadings[, columns]
  pcobj$scores[, columns]   <- -scale * pcobj$scores[, columns]
  pcobj
}

#' @rdname reflect
#' @export
reflect.PCA <- function(pcobj, columns = 1:2, scale = 1, ...) {
  check_reflect_columns(pcobj$var$coord, columns)
  pcobj$var$coord[, columns] <- -scale * pcobj$var$coord[, columns]
  pcobj$ind$coord[, columns] <- -scale * pcobj$ind$coord[, columns]
  # reflect quanti.sup$coord if that is present
  if ("quanti.sup" %in% names(pcobj)) {
    pcobj$quanti.sup$coord[, columns] <- -scale * pcobj$quanti.sup$coord[, columns]
  }
  pcobj
}

#' @rdname reflect
#' @export
reflect.lda <- function(pcobj, columns = 1:2, scale = 1, ...) {
  # lda objects don't have a scores (x) component. They come from predict()
  u <- predict(pcobj)$x
  check_reflect_columns(u, columns)
  pcobj$scaling[, columns] <- -scale * pcobj$scaling[, columns]
  pcobj
}

#' @rdname reflect
#' @export
reflect.data.frame <- function(pcobj, columns, scale = 1, ...) {
  check_reflect_columns(pcobj, columns)
  pcobj[, columns] <- -scale * pcobj[, columns]
  pcobj
}

#' @rdname reflect
#' @export
reflect.matrix <- function(pcobj, columns, scale = 1, ...) {
  check_reflect_columns(pcobj, columns)
  pcobj[, columns] <- -scale * pcobj[, columns]
  pcobj
}

#' @rdname reflect
#' @export
reflect.list <- function(pcobj, columns, scale = 1, ...) {
  lapply(pcobj, reflect, columns = columns, scale = scale, ...)
}

#' @rdname reflect
#' @export
reflect.default <- function(pcobj, ...) {
  stop('No reflect() method for objects of class "', paste(class(pcobj), collapse = "/"),
       '". Expected "prcomp", "princomp", "PCA", "lda", "data.frame", "matrix", or "list".')
}
