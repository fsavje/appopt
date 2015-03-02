bigdata <- 1000000
normaldata <- 50000
smalldata <- 75

block_size1 <- 2L
block_size2 <- 4L

### Check ANN library
set <- list(list(treetype = 1L, bs = block_size1, data_size = normaldata),
            list(treetype = 2L, bs = block_size1, data_size = bigdata),
            list(treetype = 3L, bs = block_size1, data_size = bigdata),
            list(treetype = 1L, bs = block_size2, data_size = normaldata),
            list(treetype = 2L, bs = block_size2, data_size = bigdata),
            list(treetype = 3L, bs = block_size2, data_size = bigdata))

for (s in set) {
  print(s)
  for (r in 1:100) {
    cat(r, " ")
    data <- matrix(rnorm(5 * s$data_size), ncol = 5)
    ann_data_ptr <- .Call("cpp_ann_init", t(data), s$treetype, 0.0, PACKAGE = "appopt")
    ret <- .Call("cpp_ann_query",
                 ann_data_ptr,
                 0L:(nrow(data) - 1L),
                 0L:(nrow(data) - 1L),
                 s$bs - 1L,
                 rnorm(1) < 0,
                 rnorm(1) < 0,
                 rnorm(1) < 0,
                 PACKAGE = "appopt")
    rm(data, ann_data_ptr, ret)
    gc()
  }
}

### Check low level blocking functions

set <- list(list(directed = TRUE, MIS_method = 1L, bs = block_size2, data_size = bigdata),
            list(directed = TRUE, MIS_method = 2L, bs = block_size2, data_size = bigdata),
            list(directed = TRUE, MIS_method = 3L, bs = block_size2, data_size = bigdata),
            list(directed = TRUE, MIS_method = 4L, bs = block_size2, data_size = normaldata),
            list(directed = TRUE, MIS_method = 5L, bs = block_size1, data_size = smalldata),
            list(directed = FALSE, MIS_method = 1L, bs = block_size2, data_size = bigdata),
            list(directed = FALSE, MIS_method = 2L, bs = block_size2, data_size = bigdata),
            list(directed = FALSE, MIS_method = 3L, bs = block_size2, data_size = bigdata),
            list(directed = FALSE, MIS_method = 4L, bs = block_size2, data_size = normaldata),
            list(directed = FALSE, MIS_method = 5L, bs = block_size1, data_size = smalldata))

for (s in set) {
  print(s)
  for (r in 1:100) {
    cat(r, " ")
    data <- matrix(rnorm(5 * s$data_size), ncol = 5)
    ann_data_ptr <- .Call("cpp_ann_init", t(data), 2L, 0.0, PACKAGE = "appopt")
    nn_indices <- as.vector(.Call("cpp_ann_query",
                                  ann_data_ptr,
                                  0L:(nrow(data) - 1L),
                                  0L:(nrow(data) - 1L),
                                  s$bs - 1L,
                                  FALSE,
                                  FALSE,
                                  FALSE,
                                  PACKAGE = "appopt")$nn_indices)
    ret <- .Call("cpp_get_blocking",
                 nrow(data),
                 s$bs,
                 nn_indices,
                 s$directed,
                 s$MIS_method,
                 PACKAGE = "appopt")
    rm(data, ann_data_ptr, nn_indices, ret)
    gc()
  }
}
