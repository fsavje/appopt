#' @useDynLib appopt cpp_get_block_stats
NULL

#' Information about blocking
#'
#' This function derives the maximum and average distance among all within-block
#' distances for a given blocking. The average is the mean within-block
#' distance in each block weighted by block size. The function also checks
#' so each block contain at least \code{block_size} units.
#'
#' @param data a numeric matrix or data frame describing the covariates
#'   of the units in the experiment.
#' @param block_size the desired minimum block size.
#' @param blocking an integer vector describing the blocking.
#'
#' @return \code{get_block_stats} returns a named numeric vector of length
#'   two, where the first item is the maximum distance and the second item
#'   is the mean.
#'
#' @examples
#'
#' # Generate data sets
#' data <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
#'
#' # Get blocking
#' blocking <- get_blocking(data, 2)
#'
#' # Get information
#' get_block_stats(data, 2, blocking$blocks)
#'
#' @export
get_block_stats <- function(data, block_size, blocking) {

  if (is.data.frame(data)) data <- as.matrix(data)
  block_size <- as.integer(block_size)[1]

  stopifnot(is.matrix(data),
            is.numeric(data),
            ncol(data) >= 1,
            nrow(data) >= 1,
            any(!is.na(data)),
            block_size >= 2,
            is.vector(blocking),
            is.integer(blocking),
            length(blocking) == nrow(data))

  # Set so index start at 0
  blocking = blocking - min(blocking)

  .Call("cpp_get_block_stats",
        t(data),
        blocking,
        max(blocking) + 1L,
        as.numeric(block_size),
        PACKAGE = "appopt")
}
