# This file tests the `get_blocking()' function
# against an R implementation found in
# `Rrep_get_blocking()' in "Rreplica.R"

source("Rreplica.R")
library("appopt")

get_data <- function() {
  data.frame(x1 = rnorm(100), x2 = rnorm(100))
}

tests <- list(list(data_gen = get_data, test_rounds = 100))

tests <- mapply(function(t, block_size) { c(t, block_size = block_size) },
                tests, rep(c(2, 4), each = length(tests)),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
tests <- mapply(function(t, directed) { c(t, directed = directed) },
                tests, rep(c(TRUE, FALSE), each = length(tests)),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
tests <- mapply(function(t, MIS_method) { c(t, MIS_method = MIS_method) },
                tests, rep(c("lexical", "1stPowOrder", "2ndPowOrder", "heuristicSearch"), each = length(tests)),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
tests <- mapply(function(t, unassinged_method) { c(t, unassinged_method = unassinged_method) },
                tests, rep(c("adjacent_search", "seed_search"), each = length(tests)),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
tests <- mapply(function(t, treetype) { c(t, treetype = treetype) },
                tests, rep(c("brute", "kdtree", "bdtree"), each = length(tests)),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)


for (test in tests) {
  cat(". ")
  for (r in 1:test$test_rounds) {
    data <- test$data_gen()
    blocking <- get_blocking(data, test$block_size, test$directed, test$MIS_method, test$unassinged_method, test$treetype)
    Rrep_blocking <- Rrep_get_blocking(data, test$block_size, test$directed, test$MIS_method, test$unassinged_method, test$treetype)

    if (!identical(blocking, Rrep_blocking)) {
      print(all.equal(blocking, Rrep_blocking))
      stop("Not identical, breaking.")
    }
  }
}
