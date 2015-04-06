
Rrep_get_greedy_blocking <- function(data,
                                     block_size,
                                     prev_blocking = NULL) {

  if (is.data.frame(data)) data <- as.matrix(data)
  block_size <- as.integer(block_size)[1]
  if (is.null(prev_blocking)) prev_blocking <- rep(1, nrow(data))

  stopifnot(is.matrix(data),
            is.numeric(data),
            ncol(data) >= 1,
            nrow(data) >= 1,
            any(!is.na(data)),
            block_size >= 2,
            is.vector(prev_blocking),
            is.integer(prev_blocking),
            any(!is.na(prev_blocking)),
            nrow(data) == length(prev_blocking))

  options <- list(block_size = block_size,
                  type = "greedy_threshold")

  dist_mat <- as.matrix(dist(data))
  max_block_label <- max(prev_blocking)
  b <- 1L
  new_blocking <- prev_blocking

  while (b <= max_block_label) {

    in_block <- which(new_blocking == b)

    if (length(in_block) >= 2 * block_size) {
      max_block_label <- max_block_label + 1L

      new_blocking[in_block] <- 0L

      block_dist_mat <- dist_mat[in_block, in_block, drop = FALSE]
      new_centers <- in_block[which(block_dist_mat == max(block_dist_mat), arr.ind = TRUE)[2, ]]

      if (new_centers[1] > new_centers[2]) {
        new_centers <- c(new_centers[2], new_centers[1])
      }

      new_blocking[new_centers[1]] <- b
      new_blocking[new_centers[2]] <- max_block_label
      in_block <- setdiff(in_block, new_centers)

      to_assign <- in_block[order(dist_mat[new_centers[1], in_block])[1:(block_size - 1L)]]
      new_blocking[to_assign] <- b
      in_block <- setdiff(in_block, to_assign)

      to_assign <- in_block[order(dist_mat[new_centers[2], in_block])[1:(block_size - 1L)]]
      new_blocking[to_assign] <- max_block_label
      in_block <- setdiff(in_block, to_assign)

      block_dist_mat <- dist_mat[new_centers, in_block, drop = FALSE]
      new_blocking[in_block] <- max_block_label
      new_blocking[in_block][block_dist_mat[1, ] <= block_dist_mat[2, ]] <- b
    } else {
      b <- b + 1L
    }
  }

  return(structure(data.frame(labels = 1:length(prev_blocking), blocks = new_blocking), options = options))

}
