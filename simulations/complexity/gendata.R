args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 3) {
  sample_size <- args[1]
  covariates <- args[2]
  sim_run <- as.integer(args[3])
} else {
  stop("Must supply three arguments.")
}

envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

outfile <- paste0(scratch_dir, "tmpdata/td-", sample_size, "-", covariates, "-", sim_run, ".RData")

if (!file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1))) {
  warning("Invalid scratch dir.")
  quit("no", 1)
} else if (!file.exists(paste0(scratch_dir, "tmpdata"))) {
  warning("Invalid tmpdata dir.")
  quit("no", 1)
} else if (file.exists(outfile)) {
  warning("Raw data already generated.")
  quit("no", 1)
}

load(paste0(scratch_dir, "seeds.RData"))

seed_to_use <- seeds[[sample_size]][[covariates]][[sim_run]]
cov_to_use <- as.integer(covariates)
ss_to_use <- as.integer(sample_size)

set.seed(seed_to_use)

raw_data <- data.frame("id" = 1L:ss_to_use)
for (c in 1L:cov_to_use) {
  raw_data <- data.frame(raw_data, x = runif(ss_to_use, min = 0, max = 10))
}

save(sample_size, covariates, sim_run, raw_data, file = outfile, compress = FALSE)
cat(paste0(outfile, "\n"))
