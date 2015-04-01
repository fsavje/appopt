#' @useDynLib appopt cpp_get_greedy_blocking
NULL

#' @export
get_greedy_blocking <- function(block_size,
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

  blocks <- .Call("cpp_get_greedy_blocking",
                  block_size,
                  prev_blocking - 1L,
                  dist_mat,
                  PACKAGE = "appopt") + 1L

  return(structure(data.frame(labels = 1:length(prev_blocking), blocks = blocks), options = options))

}
