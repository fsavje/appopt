source("Rreplica_estimate.R")
library("appopt")

set.seed(19860604)

for (t in 1:1000) {

  num_blocks <- sample(1:100, 1)
  block_size <- sample(2:6, 1)
  extras <- sample(1:(num_blocks * block_size), 1)
  if (block_size == 2) {
    treatments_levels <- 1:2
  } else {
    treatments_levels <- 1:sample(2:block_size, 1)
  }

  blocking <- sample(c(rep(1:num_blocks, block_size), sample(1:num_blocks, extras, replace = TRUE)))
  treatments <- get_randomize(blocking, treatments_levels)

  contrast <- sample(treatments_levels, 2)

  f_gen <- rnorm(2)
  f_outcomes <- numeric(length = length(blocking))
  f_outcomes[treatments == contrast[1]] <- f_gen[1]
  f_outcomes[treatments == contrast[2]] <- f_gen[2]

  r_outcomes <- rnorm(length(blocking))

  f_true <- f_gen[1] - f_gen[2]
  f_est <- get_estimate(f_outcomes, blocking, treatments, contrast)
  Rrep_f_est <- Rrep_estimate(f_outcomes, blocking, treatments, contrast)

  r_est <- get_estimate(r_outcomes, blocking, treatments, contrast)
  Rrep_r_est <- Rrep_estimate(r_outcomes, blocking, treatments, contrast)

  if (!isTRUE(all.equal(f_true, f_est))) {
    print(all.equal(f_true, f_true))
    stop("Not identical 1, breaking.")
  }
  if (!isTRUE(all.equal(f_est, Rrep_f_est))) {
    print(all.equal(f_true, f_true))
    stop("Not identical 2, breaking.")
  }
  if (!isTRUE(all.equal(r_est, Rrep_r_est))) {
    print(all.equal(f_true, f_true))
    stop("Not identical 3, breaking.")
  }

}
