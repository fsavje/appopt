load(commandArgs(trailingOnly = TRUE)[1])

suppressPackageStartupMessages(library("blockTools"))




blocking <- block(raw_data, n.tr = 2L, id.vars = "id", algorithm = "optGreedy")


cat("moore",  sample_size, covariates, sim_run, "- ")
