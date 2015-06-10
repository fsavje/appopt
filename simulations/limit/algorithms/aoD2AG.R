load(commandArgs(trailingOnly = TRUE)[1])

suppressPackageStartupMessages(library("appopt"))

raw_data$id <- NULL
raw_data <- normalize_cov(raw_data)

blocking <- get_blocking(raw_data, 2L, TRUE,  "2ndPowOrder", "adjacent_search", "kdtree")
blocking <- get_greedy_blocking(raw_data, 2L, blocking$blocks)

cat("aoD2AG", sample_size, covariates, sim_run, "- ")
