#' appopt: An approximate optimal threshold blocking algorithm
#'
#' Package to derive approximately optimal threshold blockings.
#' Currently includes functions \code{\link{normalize_cov}} and
#' \code{\link{get_blocking}}.
#'
#' This package is under heavy development, please use great
#' caution when using it.
#'
#' More information and the latest version is found here:
#' \url{http://github.com/fsavje/appopt}.
#'
#' Bug reports and suggestions are greatly appreciated and
#' are best reported here:
#' \url{http://github.com/fsavje/appopt/issues}.
#'
#' @docType package
#' @name appopt-package
#' @aliases appopt
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The `appopt' package is under heavy development, please use great caution when using it.")
  packageStartupMessage("Bug reports and suggestions are greatly appreciated and are best reported here: http://github.com/fsavje/appopt/issues")
}

