load(commandArgs(trailingOnly = TRUE)[1])

suppressPackageStartupMessages(library("nbpMatching"))




blocking <- nonbimatch(distancematrix(gendistance(raw_data, idcol = "id")), precision = 8)


cat("nbpm",   sample_size, covariates, sim_run, "- ")
