Rrep_get_block_stats <- function(data, block_size, blocking) {

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

  max_dist <- 0
  avg_dist <- 0

  dist_mat <- as.matrix(dist(data))

  for (b in unique(blocking)) {
    if (sum(blocking == b) < block_size) error("Invalid blocking.")
    sub_mat <- dist_mat[blocking == b, blocking == b]
    max_dist <- max(max_dist, sub_mat)
    avg_dist <- avg_dist + sum(blocking == b) * mean(sub_mat) / length(blocking)
  }

  c("max" = max_dist, "mean" = avg_dist)
}
