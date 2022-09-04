# proto -> ggproto

GeomCircle <- ggplot2::ggproto(ggplot2:::Geom, expr={
  objname <- "circle"

  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(
    colour = "black", 
    size = 0.5, 
    linetype = 1, 
    fill = NA, 
    alpha = NA
  )
  required_aes <- c("x", "y", "radius")

  guide_geom <- function(.) "polygon"

  draw <- function(., data, scales, coordinates, ...) {
    nsegment <- 51
    if (empty(data)) return(zeroGrob())

    if (!is.linear(coordinates)) {
      warning("geom_circle does not work properly with non-linear coordinates.")
    }

    data <- unique(data)

    circle <- data.frame(x = cospi(seq(0, 2, length.out = nsegment)),
                         y = sinpi(seq(0, 2, length.out = nsegment)))

    grobs <- lapply(seq_len(nrow(data)), function(i) {
      df <- transform(data[rep(i, nrow(circle)), ],
                      x = x + radius * circle$x, 
                      y = y + radius * circle$y,
                      radius = NULL)
      with(coord_transform(coordinates, df, scales),
        polygonGrob(x, y, default.units = "native",
          gp = gpar(col = alpha(colour, alpha),
                    fill = fill,
                    lwd = size * .pt,
                    lty = linetype)
        )
      )
    })

    do.call("gList", grobs)
  }
})

#' Circles specified by position and radius
#'
#' @param mapping  Aesthetic mappings, inherited
#' @param data     The data to be displayed in this layer; by default data is inherited from the plot data
#' @param stat     The statistical transformation to use on the data for this layer, as a string
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ...      Other arguments passed down
#' @export
geom_circle <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", ...) {

  GeomCircle$new(mapping = mapping, data = data, stat = stat,
    position = position, ...)
}
