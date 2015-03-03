# This file tests the ANN wrapper against
# an R implementation found in `Rrep_nn_query()'
# in "Rreplica.R"

source("Rreplica.R")
library("appopt")

get_data <- function() {
  data.frame(x1 = rnorm(100), x2 = rnorm(100))
}
test_rounds <- 100
sub_rounds <- 10

for (r in 1:test_rounds) {
  cat(". ")
  data <- get_data()
  n <- nrow(data)
  treetype <- sample(1:3, 1)
  sindices <- 0:(n - 1)
  qindices <- 0:(n - 1)
  ann_data_ptr <- .Call("cpp_ann_init", t(data), treetype, 0.0, PACKAGE = "appopt")

  for (sr in 1:sub_rounds) {
    return_distances <- (rnorm(1) < 0)
    selfmatch <- (rnorm(1) < 0)
    common_search <- (rnorm(1) < 0)

    search_type <- sample(1:4, 1)
    if (search_type == 1) {
      k <- sample(1:min(10, n - 1), 1)
      sindices <- 0:(n - 1)
      qindices <- 0:(n - 1)
    } else if (search_type == 2) {
      k <- sample(1:min(10, n - 1), 1)
      sindices <- sample(0:(n - 1), sample((k + 1):n, 1))
      qindices <- sample(0:(n - 1), sample(1:n, 1))
    } else if (search_type == 3) {
      k <- sample(1:min(10, length(sindices) - 1), 1)
      qindices <- sample(0:(n - 1), sample(1:n, 1))
      common_search <- (rnorm(1) < 0.5) # Same search, increase prop of common_search
    } else {
      k <- sample(1:min(10, length(sindices) - 1), 1)
      common_search <- (rnorm(1) < 0.5) # Same search, increase prop of common_search
    }

    cpp_nn <-  .Call("cpp_ann_query", ann_data_ptr,
                     sindices, qindices,
                     k, return_distances,
                     selfmatch, common_search,
                     PACKAGE = "appopt")

    Rrep_nn <- Rrep_nn_query(data, sindices, qindices, k, return_distances, selfmatch)

    if (!return_distances && !identical(cpp_nn, Rrep_nn)) {
      print(all.equal(cpp_nn, Rrep_nn))
      stop("Not identical, breaking.")
    } else if (return_distances && !isTRUE(all.equal(cpp_nn, Rrep_nn))) {
      # On some platforms distances (double) may not be bitwise identical
      print(all.equal(cpp_nn, Rrep_nn))
      stop("Not identical, breaking.")
    }
  }
}
