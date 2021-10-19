.onLoad <- function(libname, pkgname) {
  opt <- getOption("silo.grid.spec")
  if (is.null(opt)) {
    options(silo.grid.spec = .default_grid.spec())
  }
  invisible(NULL)
}
