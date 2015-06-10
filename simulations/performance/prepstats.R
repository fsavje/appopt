envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

stopifnot(file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1)),
          file.exists(paste0(scratch_dir, "collected")))

if (!file.exists(paste0(scratch_dir, "statsims"))) dir.create(paste0(scratch_dir, "statsims"))

prep_sim <- function(collect_name, methods) {
  collect_file <- paste0(scratch_dir, "collected/", collect_name, ".RData")
  outfile <- paste0(scratch_dir, "statsims/", collect_name, ".RData")
  
  stopifnot(file.exists(collect_file),
            !file.exists(outfile))
  
  load(collect_file)
  
  ready <- lapply(collected, function(batch) {
    batch$blockings <- batch$blockings[intersect(names(batch$blockings), methods)]
    batch
  })
  
  save(ready, settings, file = outfile, compress = FALSE)
  invisible(NULL)
}

data_types <- c("ad-100-2", "ad-100-4", "ad-500-2", "ad-500-4", "ad-1k-2", "ad-1k-4", "ad-5k-2", "ad-5k-4", "ad-10k-2", "ad-10k-4")
methods <- c("moore", "nbpmatch", "greedy", "appopt_ULAV", "appopt_DLAV", "appopt_D2AG", "appopt_D2SG")

invisible(lapply(data_types, prep_sim, methods))
