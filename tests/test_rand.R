source("Rreplica_randomize.R")
library("appopt")

set.seed(19860604)

for (t in 1:1000) {

  num_blocks <- sample(1:100, 1)
  block_size <- sample(2:6, 1)
  extras <- sample(1:(num_blocks * block_size), 1)
  if (block_size == 2) {
    treatments <- 1:2
  } else {
    treatments <- 1:sample(2:block_size, 1)
  }

  blocking <- sample(c(rep(1:num_blocks, block_size), sample(1:num_blocks, extras, replace = TRUE)))

  save_seed <- get(".Random.seed", envir = globalenv())
  rand_treat <- get_randomize(blocking, treatments)
  new_seed <- get(".Random.seed", envir = globalenv())
  assign(".Random.seed", save_seed, envir = globalenv())
  Rrep_rand_treat <- Rrep_randomize(blocking, treatments)
  assign(".Random.seed", new_seed, envir = globalenv())

  if (!identical(rand_treat, Rrep_rand_treat)) {
    print(all.equal(rand_treat, Rrep_rand_treat))
    stop("Not identical, breaking.")
  }

  stopifnot(all(sapply(unique(blocking), function(b) {
    treat_inblock <- table(rand_treat[blocking == b])
    if (sum(blocking == b) %% length(treatments) == 0) {
      return(all(treat_inblock == treat_inblock[1]))
    } else {
      treat_inblock <- treat_inblock - min(treat_inblock)
      return((sum(treat_inblock) == (sum(blocking == b) %% length(treatments))) &&
               (max(treat_inblock) == 1))
    }
  })))

}

blocking <- rep(1:100, each = 4)
rand_treat12 <- rowSums(sapply(1:300000, function(r) { get_randomize(blocking, 1:2) == 1 }))
rand_treat123 <- rowSums(sapply(1:300000, function(r) { get_randomize(blocking, 1:3) == 1 }))
stopifnot(max(abs(rand_treat12 / 300000 - 0.5)) < 0.01)
stopifnot(max(abs(rand_treat123 / 300000 - 1/3)) < 0.01)

rand_treat12 <- rand_treat123 <- rep(0, 500)
total <- 0
for (t in 1:10000) {
  blocking <- sample(c(rep(1:100, 3), sample(1:100, 200, replace = TRUE)))
  rand_treat12 <- rand_treat12 + rowSums(sapply(1:10, function(r) { get_randomize(blocking, 1:2) == 1 }))
  rand_treat123 <- rand_treat123 + rowSums(sapply(1:10, function(r) { get_randomize(blocking, 1:3) == 1 }))
  total <- total + 10
}
stopifnot(max(abs(rand_treat12 / total - 0.5)) < 0.01)
stopifnot(max(abs(rand_treat123 / total - 1/3)) < 0.01)



