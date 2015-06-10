envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

stopifnot(file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1)),
          file.exists(paste0(scratch_dir, "collected")))

if (!file.exists(paste0(scratch_dir, "estsims"))) dir.create(paste0(scratch_dir, "estsims"))

prep_sim <- function(collect_name, prefix, outcome_model, methods) {
  collect_file <- paste0(scratch_dir, "collected/", collect_name, ".RData")
  outfile <- paste0(scratch_dir, "estsims/", prefix, collect_name, ".RData")
  
  stopifnot(file.exists(collect_file),
            !file.exists(outfile))
  
  load(collect_file)
  
  ready <- lapply(collected, function(batch) {
    batch$blockings <- batch$blockings[intersect(names(batch$blockings), methods)]
    batch$raw_data$id <- NULL
    batch$raw_data <- outcome_model(batch$raw_data)
    batch
  })
  
  settings$outcome_model <- paste0(prefix, collect_name)
  
  save(ready, settings, file = outfile, compress = FALSE)
  invisible(NULL)
}

est_model <- function(df) {
  df <- data.frame(y0 = df$x1 * df$x2 + rnorm(nrow(df), sd = 1), df)
  df$y1 <- df$y0
  df
}

set.seed(19860604)

data_types <- c("ad-100-2", "ad-100-4", "ad-500-2", "ad-500-4", "ad-1k-2", "ad-1k-4", "ad-5k-2", "ad-5k-4", "ad-10k-2", "ad-10k-4")
methods <- c("dry", "moore", "nbpmatch", "greedy", "appopt_ULAV", "appopt_DLAV", "appopt_D2AG", "appopt_D2SG")

invisible(lapply(data_types, prep_sim, "est-", est_model, methods))
