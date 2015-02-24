#' @useDynLib appopt c_ann_init c_ann_query
#' @import Matrix
NULL

#' Construct threshold blocking
#'
#' This function derives an approximately optimal solution to the
#' threshold blocking problem where the objective is to minimize
#' the maximum Euclidean distances between any two units
#' within the same block.
#'
#' @param data a numeric matrix or data frame describing the covariates
#'   of the units in the experiment.
#' @param block_size the desired minimum block size.
#' @param algorithm the version of the algorithm to use. Must be one of
#'   "directed", "undirected" or "paper".
#' @param treetype the method to find nearest neighbors with. Must be one
#'   of "brute", "kdtree" or "bdtree". "kdtree" is recommended in most cases
#'   but for small samples or high dimensional covariates "brute" might perform
#'   better.
#'
#' @return \code{get_blocking} returns a data frame where one variable is the
#'   units' labels (\code{1:n}) and one variable is their derived block membership.
#'   Details about the used algorithm is added as attributes to the data frame.
#'
#' @examples
#'
#' data <- data.frame(x1 = rnorm(200),
#'                    x2 = rnorm(200),
#'                    x3 = runif(200))
#'
#' get_blocking(data, 4)
#'
#' @export
get_blocking <- function(data,
                         block_size,
                         algorithm = "directed",
                         #MIS_algorithm = "default",
                         #split_algorithm = "default",
                         treetype = "kdtree") {

  if (is.data.frame(data)) data <- as.matrix(data)
  block_size <- as.integer(block_size)[1]
  algorithm <- match.arg(algorithm, c("directed", "undirected", "paper"))
  MIS_algorithm <- NULL
  #MIS_algorithm <- match.arg(MIS_algorithm, c("default", "LexMIS", "MaxIS"))
  #split_algorithm <- match.arg(split_algorithm, c("default"))
  treetype <- switch(match.arg(treetype, c("brute", "kdtree", "bdtree")),
                     "brute" = 1L, "kdtree" = 2L, "bdtree" = 3L)
  eps = 0 # approximation level in the ANN library, if eps > 0 non-exact NNG

  stopifnot(is.matrix(data),
            is.numeric(data),
            ncol(data) >= 1,
            nrow(data) >= 1,
            any(!is.na(data)),
            block_size >= 2)

  # TODO: for high covariate dimensions brute might be better, check for which dimensions this is.

  ann_data_ptr <- .Call("c_ann_init", t(data), treetype, eps, PACKAGE = "appopt")

  if (algorithm == "directed") {
    blocks <- get_appopt_directed(ann_data_ptr, nrow(data), block_size, MIS_algorithm)
  } else if (algorithm == "undirected") {
    blocks <- get_appopt_undirected(ann_data_ptr, nrow(data), block_size, MIS_algorithm)
  } else if (algorithm == "paper") {
    blocks <- get_appopt_paper(ann_data_ptr, nrow(data), block_size, MIS_algorithm)
  }

  structure(data.frame(labels = 1:nrow(data), blocks = blocks),
            options = list(block_size = block_size,
                           algorithm = algorithm)) #,
                           #MIS_algorithm = MIS_algorithm,
                           #split_algorithm = split_algorithm))
}

get_appopt_undirected <- function(ann_data_ptr,
                                  n,
                                  block_size,
                                  MIS_algorithm) {

  blocks <- vector(mode = "integer", length = n)

  nn_indices <- as.vector(get_knn(ann_data_ptr, 0:(n - 1), 0:(n - 1), block_size - 1, FALSE))
  NNE <- sparseMatrix(i = nn_indices + 1,
                      j = rep(1:n, each = (block_size - 1)),
                      dims = c(n, n),
                      symmetric = TRUE)

  SA <- (NNE %*% NNE) | NNE
  seeds <- vector(mode = "integer", length = 0)
  candidates <- rep(TRUE, times = n)
  for (i in 1:n) {
    if (candidates[i]) {
      seeds <- c(seeds, i)
      candidates[SA[, i]] <- FALSE
    }
  }

  for (b in seq_along(seeds)) {
    blocks[seeds[b]] <- b
    blocks[NNE[, seeds[b]]] <- b
  }

  if (any(blocks == 0)) {
    blocks[blocks == 0] <- as.vector(get_knn(ann_data_ptr, seeds - 1L, which(blocks == 0) - 1L, 1L, TRUE))
  }

  return(blocks)
}

get_appopt_paper <- function(ann_data_ptr,
                             n,
                             block_size,
                             MIS_algorithm) {

  blocks <- vector(mode = "integer", length = n)

  # Step 1: Get (k - 1)-nearest neighbor graph

  nn_indices <- as.vector(get_knn(ann_data_ptr, 0:(n - 1), 0:(n - 1), block_size - 1, FALSE))
  NNE <- sparseMatrix(i = nn_indices + 1,
                      j = rep(1:n, each = (block_size - 1)),
                      dims = c(n, n),
                      symmetric = TRUE)

  # Step 2: Find MIS in the second power (lexicographical first)

  SA <- (NNE %*% NNE) | NNE
  seeds <- vector(mode = "integer", length = 0)
  candidates <- rep(TRUE, times = n)
  for (i in 1:n) {
    if (candidates[i]) {
      seeds <- c(seeds, i)
      candidates[SA[, i]] <- FALSE
    }
  }

  # Step 3: Form blocks with the seeds adjacent vertices

  for (b in seq_along(seeds)) {
    blocks[seeds[b]] <- b
    blocks[NNE[, seeds[b]]] <- b
  }

  # Step 4: Assign remaining vertices to the block
  # which contain a (k - 1)-nearest neighbor (lexicographical ordering)

  in_blocks <- (blocks != 0)
  for (r in which(!in_blocks)) {
    blocks[r] <- blocks[NNE[, r] & in_blocks][1]
  }

  return(blocks)
}

get_appopt_directed <- function(ann_data_ptr,
                                n,
                                block_size,
                                MIS_algorithm) {

  blocks <- vector(mode = "integer", length = n)

  nn_indices <- as.vector(get_knn(ann_data_ptr, 0:(n - 1), 0:(n - 1), block_size - 1, FALSE))
  NNE <- sparseMatrix(i = nn_indices + 1,
                      j = rep(1:n, each = (block_size - 1)),
                      dims = c(n, n))

  SA <-  NNE | t(NNE) | (t(NNE) %*% NNE)
  seeds <- vector(mode = "integer", length = 0)
  candidates <- rep(TRUE, times = n)
  for (i in 1:n) {
    if (candidates[i]) {
      seeds <- c(seeds, i)
      candidates[SA[, i]] <- FALSE
    }
  }

  for (b in seq_along(seeds)) {
    blocks[seeds[b]] <- b
    blocks[NNE[, seeds[b]]] <- b
  }

  if (any(blocks == 0)) {
    blocks[blocks == 0] <- as.vector(get_knn(ann_data_ptr, seeds - 1L, which(blocks == 0) - 1L, 1L, TRUE))
  }

  return(blocks)
}

get_knn <- function(ann_data_ptr, search, query, k, selfmatch) {
  .Call("c_ann_query",
        ann_data_ptr,
        search,
        query,
        k,
        FALSE,
        selfmatch,
        FALSE,
        PACKAGE = "appopt")$nn_indices
}
