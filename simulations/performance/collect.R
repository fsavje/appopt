envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

stopifnot(file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1)),
          file.exists(paste0(scratch_dir, "done")))

if (!file.exists(paste0(scratch_dir, "collected"))) dir.create(paste0(scratch_dir, "collected"))


collect_run <- function(prefix, ss, bs, n_rounds) {
  
  name <- paste0(prefix, "-", ss, "-", bs)
  outfile <- paste0(scratch_dir, "collected/", name, ".RData")
  
  stopifnot(!file.exists(outfile))
  
  ss_to_use <- unname(c("100" = 10e1L, "500" = 50e1L, "1k" = 10e2L, "5k" = 50e2L, "10k" = 10e3L)[ss])
  bo <- NULL
  
  collected <- lapply(1L:n_rounds, function(r) {
    batchfile <- paste0(scratch_dir, "done/", name, "-", r, ".RData")
    stopifnot(file.exists(batchfile))
    load(batchfile)
    if (is.null(bo)) bo <<- block_on
    
    stopifnot(paste0(name, "-", r, ".RData") == batch_name,
              ss_to_use == nrow(blockings),
              ss_to_use == nrow(raw_data),
              bs == block_size,
              bo == block_on)
	
    list(blockings = blockings,
         raw_data = raw_data)
  })
  
  settings <- list(sample_size = ss_to_use,
                   sample_size_t = ss,
                   n_rounds = n_rounds,
                   block_size = bs,
                   block_on = bo)
  
  save(collected, settings, file = outfile, compress = FALSE)
}


#n_rounds <- 10L
n_rounds <- 5000L

collect_run("ad", "100", 2L, n_rounds)
collect_run("ad", "100", 4L, n_rounds)
collect_run("ad", "500", 2L, n_rounds)
collect_run("ad", "500", 4L, n_rounds)
collect_run("ad", "1k", 2L, n_rounds)
collect_run("ad", "1k", 4L, n_rounds)
collect_run("ad", "5k", 2L, n_rounds)
collect_run("ad", "5k", 4L, n_rounds)
collect_run("ad", "10k", 2L, n_rounds)
collect_run("ad", "10k", 4L, n_rounds)

