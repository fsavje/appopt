source("Rreplica_greedy.R")
library("appopt")

for (t in 1:10) {
  data <- data.frame(x1 = rnorm(1000), x2 = rnorm(1000))
  block_size <- as.integer(runif(1, min = 2, max = 10))

  blocking_old1 <- get_blocking(data, 2, MIS_method = "lexical")$blocks
  blocking_old2 <- rep(1L, 1000)

  block1 <- get_greedy_blocking(data, block_size, blocking_old1)
  Rrep_block1 <- Rrep_get_greedy_blocking(data, block_size, blocking_old1)

  if (!identical(block1, Rrep_block1)) {
    print(all.equal(block1, Rrep_block1))
    stop("Not identical, breaking 1.")
  }

  block2 <- get_greedy_blocking(data, block_size, blocking_old2)
  Rrep_block2 <- Rrep_get_greedy_blocking(data, block_size, blocking_old2)

  if (!identical(block2, Rrep_block2)) {
    print(all.equal(block2, Rrep_block2))
    stop("Not identical, breaking 2.")
  }
}
