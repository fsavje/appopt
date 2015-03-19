source("Rreplica_stats.R")
library("appopt")

for (t in 1:1000) {
  data <- data.frame(x1 = rnorm(1000), x2 = rnorm(1000))
  blocking <- get_blocking(data, 2, MIS_method = "lexical")
  res <- get_block_stats(data, 2, blocking$blocks)
  Rrep_res <- Rrep_get_block_stats(data, 2, blocking$blocks)
  if (!isTRUE(all.equal(res, Rrep_res))) {
    print(all.equal(cpp_nn, Rrep_nn))
    stop("Not identical, breaking.")
  }
}

