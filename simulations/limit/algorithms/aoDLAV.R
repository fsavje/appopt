load(commandArgs(trailingOnly = TRUE)[1])

suppressPackageStartupMessages(library("appopt"))

raw_data$id <- NULL
raw_data <- normalize_cov(raw_data)

blocking <- get_blocking(raw_data, 2L, TRUE,  "lexical",     "adjacent_search", "kdtree")


cat("aoDLAV", sample_size, covariates, sim_run, "- ")
