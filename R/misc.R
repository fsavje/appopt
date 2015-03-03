#' Normalize and weight data sets
#'
#' This function normalizes and weights data sets. A data set (data frame or matrix)
#' of \code{n} rows and \code{m} columns is supplied and a matrix of the same
#' dimensions is returned with normalized and weighted columns.
#'
#' If \code{norm_cov}
#' is the (estimated) covariance matrix and \code{normalize == "decorrelate"} then
#' the Euclidean distances between rows in the returned matrix is equivalent to
#' Mahalanobis distances between the corresponding rows in the original matrix.
#' With the same \code{norm_cov} but with \code{normalize == "sd_normalize"},
#' the Euclidean distances in the returned matrix is equivalent to normalized
#' Euclidean distances in the original matrix.
#'
#' @param x a numeric matrix or data frame.
#' @param normalize the type of normalization to be done. This must
#'   be one of "none", "decorrelate" or "sd_normalize". "decorrelate"
#'   normalizes with \code{norm_cov} and "sd_normalize" normalizes with
#'   the diagonal of \code{norm_cov}.
#' @param norm_cov a numeric vector or matrix of the same dimensions
#'   as columns in \code{x} to be used for normalization. If a vector
#'   a diagonal matrix is constructed. If \code{NULL} the sample
#'   covariance is used, \code{norm_cov = var(x)}.
#' @param weights a numeric vector or matrix of the same dimensions
#'   as columns in \code{x} with weights for each covariate. If a vector
#'   a diagonal matrix is constructed. \code{NULL}
#'   gives no weighting (i.e., a vector or ones).
#'
#' @return \code{normalize_cov} returns a matrix with normalized and
#'   weighted columns.
#'
#' @examples
#'
#' data <- matrix(rnorm(600), ncol = 3)
#' data[, 2] <- data[, 1] + data[, 2]
#' data[, 3] <- (data[, 1] + 1) * data[, 3]
#'
#' round(var(data), digits = 3)
#'
#' data <- normalize_cov(data)
#'
#' round(var(data), digits = 3)
#'
#' @export
normalize_cov <- function(x, normalize = "decorrelate",
                          norm_cov = NULL, weights = NULL) {

  normalize <- match.arg(normalize, c("none", "decorrelate", "sd_normalize"))

  if (is.data.frame(x)) x <- as.matrix(x)
  stopifnot(is.matrix(x),
            is.numeric(x))

  if (normalize != "none") {
    if (is.null(norm_cov)) norm_cov <- var(x)
    if (is.data.frame(norm_cov)) norm_cov <- as.matrix(norm_cov)
    if (is.vector(norm_cov)) norm_cov <- diag(norm_cov)
    if (normalize == "sd_normalize") norm_cov <- diag(diag(norm_cov))
    stopifnot(is.matrix(norm_cov),
              is.numeric(norm_cov),
              all(!is.na(norm_cov)),
              isSymmetric(norm_cov),
              ncol(norm_cov) == ncol(x))
    if (any(eigen(norm_cov, symmetric = TRUE, only.values = TRUE)$values <= 2 * .Machine$double.eps)) {
      stop("`norm_cov' must be positive-semidefinite.")
    }

    x <- tcrossprod(x, chol(solve(norm_cov)))
  }

  if (!is.null(weights)) {
    if (is.data.frame(weights)) weights <- as.matrix(weights)
    if (is.vector(weights)) weights <- diag(weights)
    stopifnot(is.matrix(weights),
              is.numeric(weights),
              all(!is.na(weights)),
              isSymmetric(weights),
              ncol(weights) == ncol(x))
    if (any(eigen(weights, symmetric = TRUE, only.values = TRUE)$values <= 2 * .Machine$double.eps)) {
      stop("`weights' must be positive-semidefinite.")
    }

    x <- tcrossprod(x, chol(weights))
  }

  return(x)
}
