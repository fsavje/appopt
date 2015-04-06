#' @useDynLib appopt cpp_get_greedy_blocking
NULL

#' @export
get_greedy_blocking <- function(data,
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

  blocks <- .Call("cpp_get_greedy_blocking",
                  block_size,
                  prev_blocking - 1L,
                  t(data),
                  PACKAGE = "appopt") + 1L

  return(structure(data.frame(labels = 1:length(prev_blocking), blocks = blocks), options = options))

}
