envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

if (!file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1))) {
  warning("Invalid scratch dir.")
  quit("no", 1)
}

if (!file.exists(paste0(scratch_dir, "b4G"))) dir.create(paste0(scratch_dir, "b4G"))
if (!file.exists(paste0(scratch_dir, "b25G"))) dir.create(paste0(scratch_dir, "b25G"))
if (!file.exists(paste0(scratch_dir, "running"))) dir.create(paste0(scratch_dir, "running"))
if (!file.exists(paste0(scratch_dir, "done"))) dir.create(paste0(scratch_dir, "done"))

gendata <- function(sample_size, all_block_sizes, n_rounds, folder) {
  ss_to_use <- unname(c("100" = 10e1L, "500" = 50e1L, "1k" = 10e2L, "5k" = 50e2L, "10k" = 10e3L)[sample_size])
  block_on <- c("x1", "x2")
  lapply(1L:n_rounds, function(r) {
    raw_data <- data.frame("id" = 1L:ss_to_use,
                           x1 = runif(ss_to_use, min = 0, max = 10),
                           x2 = runif(ss_to_use, min = 0, max = 10))
    lapply(all_block_sizes, function(block_size) {
      filename <- paste0(scratch_dir, folder, "/ad-", sample_size, "-", block_size, "-", r, ".RData")
      if (file.exists(filename)) {
        warning("Raw data already generated.")
        quit("no", 1)
      } else {
        save(raw_data, block_size, block_on, file = filename, compress = FALSE)
      }
    })
  })
  invisible(NULL)
}

set.seed(19860604)
#n_rounds <- 10L
n_rounds <- 5000L

gendata("100", c(2L, 4L), n_rounds, "b4G")
gendata("500", c(2L, 4L), n_rounds, "b4G")
gendata("1k", c(2L, 4L), n_rounds, "b4G")
gendata("5k", c(2L, 4L), n_rounds, "b25G")
gendata("10k", c(2L, 4L), n_rounds, "b25G")
