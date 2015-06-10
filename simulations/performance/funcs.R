get_moore <- function(data, block_size) {
  blocking <- block(data, n.tr = block_size, id.vars = "id", algorithm = "optGreedy")
  
  blocking <- blocking$blocks[[1]]
  blocking <- blocking[grep("Unit ", colnames(blocking))]
  out_blocks <- vector(mode = "integer", length = nrow(data))
  lapply(blocking, function(u) { out_blocks[as.integer(u)] <<- 1:length(u) })
  out_blocks
}

get_nbpmatch <- function(data) {
  blocking <- nonbimatch(distancematrix(gendistance(data, idcol = "id")), precision = 8)
  
  blocking <- blocking$halves
  blocking <- blocking[grep(".Row", colnames(blocking))]
  out_blocks <- vector(mode = "integer", length = nrow(data))
  lapply(blocking, function(u) { out_blocks[u] <<- 1:length(u) })
  out_blocks
}

get_greedy <- function(data, block_size) {
  data$id <- NULL
  data <- normalize_cov(data)
  blocking <- get_greedy_blocking(data, block_size)
  blocking$blocks
}

get_appopt <- function(data, block_size, directed, MIS_method, unassinged_method, greedy_refine) {
  data$id <- NULL
  data <- normalize_cov(data)
  blocking <- get_blocking(data, block_size, directed = directed, MIS_method = MIS_method, unassinged_method = unassinged_method)
  if (greedy_refine) {
    blocking <- get_greedy_blocking(data, block_size, blocking$blocks)
  }
  blocking$blocks
}
