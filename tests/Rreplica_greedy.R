
Rrep_get_greedy_blocking <- function(block_size,
                                     prev_blocking,
                                     dist_mat) {

  block_size <- as.integer(block_size)[1]
  if (is.data.frame(dist_mat)) dist_mat <- as.matrix(dist_mat)
  if (class(dist_mat) == "dist") dist_mat <- as.matrix(dist_mat)

  stopifnot(block_size >= 2L,
            is.vector(prev_blocking),
            is.integer(prev_blocking),
            any(!is.na(prev_blocking)),
            is.matrix(dist_mat),
            is.numeric(dist_mat),
            ncol(dist_mat) == length(prev_blocking),
            nrow(dist_mat) == length(prev_blocking),
            any(!is.na(dist_mat)))


  options <- list(block_size = block_size,
                  type = "greedy_threshold")

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
