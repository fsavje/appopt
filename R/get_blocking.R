#' @useDynLib appopt cpp_ann_init cpp_ann_query cpp_get_blocking
NULL

#' Construct threshold blocking
#'
#' This function produces an approximately optimal solution to the
#' threshold blocking problem where the objective is to minimize
#' the maximum Euclidean distances between any two units
#' within the same block.
#'
#' @section Large data sets:
#'
#' \code{get_blocking} works well with large data sets (several millions of
#' data points) but some parameter values can lead to extensive run time.
#' Generally \code{algorithm == "directed"} is slightly faster than
#' \code{algorithm == "undirected"}. For data sets with less than
#' 100,000 data points \code{MIS_method == "heuristicSearch"} usually
#' works well. For bigger data sets (less than ten million points)
#' \code{MIS_method == "2ndPowOrder"} is to prefer. Very large data sets
#' should consider "lexical" or "1stPowOrder". \code{MIS_method == "MaxIS"}
#' is usually only feasible with data sets with data points in the lower hundreds.
#'
#' The function is fastest with \code{algorithm == "directed"} and
#' \code{MIS_method == "lexical"}. The time complexity is then
#' \eqn{O(kn log n)}. Using "undirected", "1stPowOrder" or
#' "2ndPowOrder" add steps taking \eqn{O(kn)} time with
#' varying constants. Calling with "heuristicSearch" adds a step
#' with \eqn{O(kn^1.5)} over "2ndPowOrder". "MaxIS" is NP-hard.
#'
#' @param data a numeric matrix or data frame describing the covariates
#'   of the units in the experiment.
#' @param block_size the desired minimum block size.
#' @param algorithm the version of the algorithm to use. Must be one of
#'   "directed", "undirected" or "paper".
#' @param MIS_method the method to find a maximal independent set with.
#'   Must be one of "lexical", "1stPowOrder", "2ndPowOrder", "heuristicSearch"
#'   or "MaxIS".
#' @param treetype the method to find nearest neighbors with. Must be one
#'   of "brute", "kdtree" or "bdtree". "kdtree" is recommended in most cases
#'   but for small samples or high dimensional covariates "brute" might perform
#'   better. For highly clustered data "bdtree" might perform best.
#'
#' @return \code{get_blocking} returns a data frame where one variable is the
#'   units' labels (\code{1:n}) and one variable is their block membership.
#'   Details about the used algorithm is added as attributes to the data frame.
#'
#' @examples
#'
#' # Generate data sets
#' x1 <- rnorm(200)
#' data <- data.frame(x1 = x1, x2 = x1 + rnorm(200), x3 = x1 * runif(200))
#' large_data <- data.frame(x1 = rnorm(5000000), x2 = runif(5000000))
#'
#' # Euclidean distances
#' blocking1 <- get_blocking(data, 4)
#'
#' # Mahalanobis distances
#' data_maha <- normalize_cov(data)
#' blocking2 <- get_blocking(data_maha, 4)
#'
#' # Change `MIS_method' for large data sets
#' blocking3 <- get_blocking(large_data, 4, MIS_method = "2ndPowOrder")
#'
#' @export
get_blocking <- function(data,
                         block_size,
                         algorithm = "directed",
                         MIS_method = "heuristicSearch",
                         #split_algorithm = "default",
                         treetype = "kdtree") {

  if (is.data.frame(data)) data <- as.matrix(data)
  block_size <- as.integer(block_size)[1]
  algorithm <- match.arg(algorithm, c("directed", "undirected", "paper"))
  MIS_method <- match.arg(MIS_method, c("lexical", "1stPowOrder", "2ndPowOrder", "heuristicSearch", "MaxIS"))
  #split_algorithm <- match.arg(split_algorithm, c("default"))
  treetype <- match.arg(treetype, c("brute", "kdtree", "bdtree"))
  eps = 0.0 # approximation level in the ANN library, if eps > 0 non-exact NNG

  stopifnot(is.matrix(data),
            is.numeric(data),
            ncol(data) >= 1,
            nrow(data) >= 1,
            any(!is.na(data)),
            block_size >= 2)

  #if (algorithm == "paper" && MIS_method != "lexical") {
  #  warning("Using `paper' algorithm but not `lexical', changing to `lexical'.")
  #  MIS_method <- "lexical"
  #}

  #split_algorithm = split_algorithm))
  options <- list(block_size = block_size,
                  algorithm = algorithm,
                  MIS_method = MIS_method,
                  treetype = treetype)

  algorithm <- switch(algorithm, "directed" = 1L, "undirected" = 2L, "paper" = 3L)
  MIS_method <- switch(MIS_method, "lexical" = 1L, "1stPowOrder" = 2L,
                       "2ndPowOrder" = 3L, "heuristicSearch" = 4L, "MaxIS" = 5L)
  treetype <- switch(treetype, "brute" = 1L, "kdtree" = 2L, "bdtree" = 3L)

  n_data_points <- nrow(data)
  u_indices <- 0L:(n_data_points - 1L)

  # Step 1: Get (k - 1)-nearest neighbor graph

  ann_data_ptr <- .Call("cpp_ann_init", t(data), treetype, eps, PACKAGE = "appopt")

  nn_indices <- as.vector(.Call("cpp_ann_query",
                                ann_data_ptr,
                                u_indices,
                                u_indices,
                                block_size - 1L,
                                FALSE, # Don't need distances, only indices
                                FALSE, # Selfmatch not allowed
                                FALSE, # Don't save, will not run search again
                                PACKAGE = "appopt")$nn_indices)

  # Step 2: Find MIS in the second power
  # Step 3: Form blocks with the seeds adjacent vertices

  blocking_from_cpp <- .Call("cpp_get_blocking",
                             n_data_points,
                             block_size,
                             nn_indices,
                             algorithm,
                             MIS_method,
                             PACKAGE = "appopt")

  seeds <- blocking_from_cpp$seeds
  blocks <- blocking_from_cpp$blocks
  unassigned <- blocking_from_cpp$unassigned

  # Step 4: Assign unassigned vertices

  if (length(unassigned) > 0 && algorithm != 3L) {
    # Assign to block with nearest seed
    blocks[unassigned + 1L] <- blocks[as.vector(.Call("cpp_ann_query",
                                                      ann_data_ptr,
                                                      seeds,
                                                      unassigned,
                                                      1L,
                                                      FALSE,
                                                      TRUE,  # Allow selfmatch (seeds & unassigned are disjoint)
                                                      FALSE,
                                                      PACKAGE = "appopt")$nn_indices) + 1L]
  }

  return(structure(data.frame(labels = 1:n_data_points, blocks = blocks), options = options))
}
