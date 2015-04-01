# In this file a complete replica of the package is
# written solely in R with the intention to check
# the C/C++ code and provide a prototype.
# It is terribly inefficient but written to be less
# error-prone and verbose than the C++ code.
#
# Rrep_get_blocking() is an exact replica of get_blocking()
# and should produce identical returns.
#
# This call:
# Rrep_ret <- Rrep_nn_query(data, sindices, qindices, k, return_distances, selfmatch)
#
# should produce the same return as calling:
# ann_data_ptr <- .Call("cpp_ann_init", t(data), *1/2/3*, 0.0, PACKAGE = "appopt")
# ret <- .Call("cpp_ann_query",
#              ann_data_ptr,
#              sindices,
#              qindices,
#              k,
#              return_distances,
#              selfmatch,
#              *FALSE/TRUE*,
#              PACKAGE = "appopt")

Rrep_get_blocking <- function(data,
                              block_size,
                              directed = TRUE,
                              MIS_method = "heuristicSearch",
                              unassinged_method = "seed_search",
                              treetype = "kdtree") {

  if (is.data.frame(data)) data <- as.matrix(data)
  block_size <- as.integer(block_size)[1]
  directed <- as.logical(directed)[1]
  MIS_method <- match.arg(MIS_method, c("lexical", "1stPowOrder", "2ndPowOrder", "heuristicSearch", "MaxIS"))
  unassinged_method <- match.arg(unassinged_method, c("adjacent_search", "seed_search"))
  treetype <- match.arg(treetype, c("brute", "kdtree", "bdtree"))

  stopifnot(is.matrix(data),
            is.numeric(data),
            ncol(data) >= 1,
            nrow(data) >= 1,
            any(!is.na(data)),
            block_size >= 2)

  options <- list(block_size = block_size,
                  type = "appopt",
                  directed = directed,
                  MIS_method = MIS_method,
                  unassinged_method = unassinged_method,
                  treetype = treetype)

  n_data_points <- nrow(data)
  v_indices_R <- 1L:n_data_points

  # Step 1: Get NNG
  nn_indices_R <- Rrep_nn_query(data,
                                v_indices_R - 1L,
                                v_indices_R - 1L,
                                block_size - 1L,
                                FALSE,
                                FALSE)$nn_indices + 1L

  NNG <- apply(nn_indices_R, 2, function (nn) { v_indices_R %in% nn })

  if (!directed) {
    NNG <- NNG | t(NNG)
  }

  # Step 2: Find MIS in the second power
  seeds_R <- Rrep_getSeeds(NNG, MIS_method)

  # Step 3: Form blocks with the seeds adjacent vertices
  blocks <- vector(mode = "integer", length = n_data_points)

  for (b in seq_along(seeds_R)) {
    blocks[seeds_R[b]] <- b
    blocks[NNG[, seeds_R[b]]] <- b
  }

  # Step 4: Assign unassigned vertices
  unassigned_R <- which(blocks == 0)

  if (length(unassigned_R) > 0) {
    if (unassinged_method == "adjacent_search") {
      is_assigned <- (blocks != 0)
      for (ua in unassigned_R) {
        # NNG[, ua] & is_assigned = assigned neighbors
        blocks[ua] <- blocks[NNG[, ua] & is_assigned][1]
      }
    } else {
      blocks[unassigned_R] <- blocks[as.vector(Rrep_nn_query(data,
                                                             seeds_R - 1L,
                                                             unassigned_R - 1L,
                                                             1L,
                                                             FALSE,
                                                             FALSE)$nn_indices) + 1L]
    }
  }

  # Check blocking
  stopifnot(all(blocks != 0),
            all(table(blocks) >= block_size))

  return(structure(data.frame(labels = 1:n_data_points, blocks = blocks), options = options))
}

Rrep_getSeeds <- function(NNG, MIS_method) {

  getMIS <- function(SA, ordering) {
    mis <- vector(mode = "integer", length = 0)
    candidates <- rep(TRUE, times = length(ordering))
    for (cand in ordering) {
      if (candidates[cand]) {
        mis <- c(mis, cand)
        candidates[SA[, cand]] <- FALSE
      }
    }
    return(mis)
  }

  n_data_points <- nrow(NNG)

  SA <- NNG | t(NNG) | (t(NNG) %*% NNG != 0)

  if (MIS_method == "lexical") {
    seeds <- getMIS(SA, 1L:n_data_points)

  } else if (MIS_method == "1stPowOrder") {
    ordering <- sort.list(colSums(NNG | t(NNG)))
    seeds <- getMIS(SA, ordering)

  } else if (MIS_method == "2ndPowOrder") {
    ordering <- sort.list(colSums(SA))
    seeds <- getMIS(SA, ordering)

  } else if (MIS_method == "heuristicSearch") {
    to_check <- 100 * as.integer(sqrt(n_data_points)) + 100;
    to_check <- min(to_check, n_data_points)

    ordering <- sort.list(colSums(SA))
    seeds <- getMIS(SA, ordering)
    for (i in 2:to_check) {
      ordering[1:i] <- c(ordering[2:i], ordering[1])
      temp_seeds <- getMIS(SA, ordering)
      if (length(temp_seeds) > length(seeds)) {
        seeds <- temp_seeds
      }
    }

  } else if (MIS_method == "MaxIS") {
    error("'MaxIS' is not implemented in the R replica.")
  }

  return(seeds)
}

Rrep_nn_query <- function(data, sindices_cpp, qindices_cpp, k, return_distances, selfmatch) {

  if (is.data.frame(data)) data <- as.matrix(data)
  k <- as.integer(k)[1]
  return_distances <- as.logical(return_distances)
  selfmatch <- as.logical(selfmatch)

  stopifnot(is.matrix(data),
            is.numeric(data),
            is.integer(sindices_cpp),
            is.integer(qindices_cpp),
            k + selfmatch <= length(sindices_cpp))

  # Get relevant distances
  dist_mat <- as.matrix(dist(data))[sindices_cpp + 1L, qindices_cpp + 1L, drop = FALSE]

  # Get ordering
  nn_order <- apply(dist_mat, 2, sort.list)

  # Get nn
  nn_indices <- apply(nn_order, 2, function (x) { sindices_cpp[x] })

  if (return_distances) {
    nn_dists <- mapply(function(dist, nn_ord) { dist[nn_ord] },
                       split(dist_mat, col(dist_mat)),
                       split(nn_order, col(nn_order)))
  }

  # Remove self if not selfmatch
  if (!selfmatch) {
    if (return_distances) {
      nn_dists <- mapply(function(nn_d, nn_i, q) { c(nn_d[nn_i != q], nn_d[nn_i == q]) },
                         split(nn_dists, col(nn_dists)),
                         split(nn_indices, col(nn_indices)),
                         qindices_cpp)
    }
    nn_indices <- mapply(function(nn_i, q) { c(nn_i[nn_i != q], nn_i[nn_i == q]) },
                         split(nn_indices, col(nn_indices)),
                         qindices_cpp)
  }

  # Get k nn
  out <- list(nn_indices = unname(nn_indices[1:k, , drop = FALSE]))
  if (return_distances) {
    out$nn_dists <- unname(nn_dists[1:k, , drop = FALSE])
  }

  return(out)
}
