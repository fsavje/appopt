load(commandArgs(trailingOnly = TRUE)[1])

suppressPackageStartupMessages(library("appopt"))

raw_data$id <- NULL
raw_data <- normalize_cov(raw_data)

blocking <- get_blocking(raw_data, 2L, TRUE,  "2ndPowOrder", "seed_search",     "kdtree")
blocking <- get_greedy_blocking(raw_data, 2L, blocking$blocks)

cat("aoD2SG", sample_size, covariates, sim_run, "- ")
