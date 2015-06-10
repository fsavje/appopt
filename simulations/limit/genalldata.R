envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

if (!file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1))) {
  stop("Invalid scratch dir.")
}

set.seed(19860604)

gendata <- function(sample_size, covariates) {
  sim_run <- 1
  cov_to_use <- as.integer(covariates)
  ss_to_use <- unname(c("100" = 10e1L,
                        "1k" = 10e2L,
                        "10k" = 10e3L,
                        "20k" = 20e3L,
                        "50k" = 50e3L,
                        "100k" = 10e4L,
                        "1M" = 10e5L,
                        "10M" = 10e6L,
                        "100M" = 10e7L)[sample_size])
  
  raw_data <- data.frame("id" = 1L:ss_to_use)
  for (c in 1L:cov_to_use) {
    raw_data <- data.frame(raw_data, x = runif(ss_to_use, min = 0, max = 10))
  }
  
  filename <- paste0(scratch_dir, "td-", sample_size, "-", covariates, ".RData")
  
  if (file.exists(filename)) {
    warning("Raw data already generated.")
    quit("no", 1)
  } else {
    save(sample_size, covariates, sim_run, raw_data,
         file = filename, compress = FALSE)
  }
}

invisible(lapply(c("50k", "100k", "10M", "100M"), function(ss) {
  lapply(c("2", "5", "10"), function(cs) {
    gendata(ss, cs)
  })
}))
