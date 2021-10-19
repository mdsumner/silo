.default_grid.spec <- function() {
 list(extent = NA,
      dimension = NA,
      projection = NA,
      resample = NULL,
      bands = NULL)
}
#' Specify the current grid
#'
#' Create or modify the details of a grid specification.
#'
#' The default grid specification is unspecified. This might change, but it's so that
#' gdalio can apply its own default based on the data in use.
#'
#' We can set the grid components explicitly  with 'silo(property_name = <values>)', such as
#'two numbers for 'dimension', or four numbers for 'extent', but these properties can also be changed
#' *relatively* (if they are already set) using functions [finer()], [coarser()] (dimension) and
#' [bigger()], [wider()], [taller()], or [smaller()], [thinner()], [shorter()].
#'
#' @param extent spatial extent xmin,xmax,ymin,ymax or a function call (see Details)
#' @param dimension physical data size,  number of columns, number of rows or a function call (see Details)
#' @param projection map projection (understood by PROJ)
#' @param resample resample algorithm (understood by GDAL warper)
#' @param bands which bands to read (1-based, can be repeated, non-sequential)
#' @param ... reserved
#'
#' @return
#' @export
#'
#' @examples
silo <- function(extent,
                      dimension,
                      projection,
                      resample = NULL,
                      bands = NULL, ...) {
  g <- g0 <- getOption("silo.grid.spec")
  print(g)
  env <- as.environment(g)
  if (!missing(dimension))  g$dimension  <- rlang::eval_tidy(rlang::enquo(dimension), env = env)
  if (!missing(extent))     g$extent     <- rlang::eval_tidy(rlang::enquo(extent), env = env)
  if (!missing(projection)) g$projection <- rlang::eval_tidy(rlang::enquo(projection), env  = env)
  if (is.null(g$bands)) g$bands <- bands
  if (is.null(g$resample)) g$resample <- resample
  options(silo.grid.spec = g)

  g0
}

finer <- function(factor = 2) {
  factor <- rep(factor, length.out = 2L)
  ceiling(dimension * factor)
}
coarser <- function(factor = 2) {
  factor <- rep(factor, length.out = 2L)
  ceiling(dimension / factor)
}
bigger <- function(factor = 2) {
  factor <- rep(factor, length.out = 2L)
  xlim <- extent[1:2]
  xlim <- xlim + (c(-1, 1) * diff(xlim)/2) * factor[1L]
  ylim <- extent[3:4]
  ylim <- ylim + (c(-1, 1) * diff(ylim)/2) * factor[1L]
  c(xlim, ylim)
}
wider <- function(factor = 2) {
  xlim <- extent[1:2]
  xlim <- xlim + (c(-1, 1) * diff(xlim)/2) * factor[1L]
  c(xlim, extent[3:4])
}
taller <- function(factor = 2) {
  ylim <- extent[3:4]
  ylim <- ylim + (c(-1, 1) * diff(ylim)/2) * factor[1L]
  c(extent[1:2], ylim)
}

smaller <- function(factor = 2) {
  bigger(factor = 1/factor)
}
thinner <- function(factor = 2) {
  wider(factor = 1/factor)
}
shorter <- function(factor = 2) {
  taller(factor = 1/factor)
}


