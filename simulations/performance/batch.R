batch_torun <- commandArgs(trailingOnly = TRUE)[1]

envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

batch_name <- unlist(strsplit(batch_torun, "/", fixed = TRUE))
batch_name <- batch_name[length(batch_name)]
outfile <- paste0(scratch_dir, "done/", batch_name)

if (!file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1))) {
  warning("Invalid scratch dir.")
  quit("no", 1)
} else if (!file.exists(paste0(scratch_dir, "running"))) {
  warning("Invalid running dir.")
  quit("no", 1)
} else if (!file.exists(paste0(scratch_dir, "done"))) {
  warning("Invalid done dir.")
  quit("no", 1)
} else if (!file.exists(batch_torun)) {
  warning("Batch do not exist.")
  quit("no", 1)
} else if (file.exists(outfile)) {
  warning("Batch results already exist.")
  quit("no", 1)
}

suppressPackageStartupMessages(library("appopt"))
suppressPackageStartupMessages(library("blockTools"))
suppressPackageStartupMessages(library("nbpMatching"))
source("funcs.R")

load(batch_torun)

blocking_data <- raw_data[c("id", block_on)]

blockings <- data.frame(dry = rep(1L, nrow(blocking_data)))

blockings$moore <- get_moore(blocking_data, block_size) 
if (block_size == 2) {
  blockings$nbpmatch <- get_nbpmatch(blocking_data)
}
blockings$greedy <- get_greedy(blocking_data, block_size)
blockings$appopt_DLSG <- get_appopt(blocking_data, block_size, TRUE,  "lexical",         "seed_search",     TRUE)
blockings$appopt_ULSG <- get_appopt(blocking_data, block_size, FALSE, "lexical",         "seed_search",     TRUE)
blockings$appopt_D2SG <- get_appopt(blocking_data, block_size, TRUE,  "2ndPowOrder",     "seed_search",     TRUE)
blockings$appopt_U2SG <- get_appopt(blocking_data, block_size, FALSE, "2ndPowOrder",     "seed_search",     TRUE)
blockings$appopt_DHSG <- get_appopt(blocking_data, block_size, TRUE,  "heuristicSearch", "seed_search",     TRUE)
blockings$appopt_UHSG <- get_appopt(blocking_data, block_size, FALSE, "heuristicSearch", "seed_search",     TRUE)
blockings$appopt_DLAG <- get_appopt(blocking_data, block_size, TRUE,  "lexical",         "adjacent_search", TRUE)
blockings$appopt_ULAG <- get_appopt(blocking_data, block_size, FALSE, "lexical",         "adjacent_search", TRUE)
blockings$appopt_D2AG <- get_appopt(blocking_data, block_size, TRUE,  "2ndPowOrder",     "adjacent_search", TRUE)
blockings$appopt_U2AG <- get_appopt(blocking_data, block_size, FALSE, "2ndPowOrder",     "adjacent_search", TRUE)
blockings$appopt_DHAG <- get_appopt(blocking_data, block_size, TRUE,  "heuristicSearch", "adjacent_search", TRUE)
blockings$appopt_UHAG <- get_appopt(blocking_data, block_size, FALSE, "heuristicSearch", "adjacent_search", TRUE)
blockings$appopt_DLSV <- get_appopt(blocking_data, block_size, TRUE,  "lexical",         "seed_search",     FALSE)
blockings$appopt_ULSV <- get_appopt(blocking_data, block_size, FALSE, "lexical",         "seed_search",     FALSE)
blockings$appopt_D2SV <- get_appopt(blocking_data, block_size, TRUE,  "2ndPowOrder",     "seed_search",     FALSE)
blockings$appopt_U2SV <- get_appopt(blocking_data, block_size, FALSE, "2ndPowOrder",     "seed_search",     FALSE)
blockings$appopt_DHSV <- get_appopt(blocking_data, block_size, TRUE,  "heuristicSearch", "seed_search",     FALSE)
blockings$appopt_UHSV <- get_appopt(blocking_data, block_size, FALSE, "heuristicSearch", "seed_search",     FALSE)
blockings$appopt_DLAV <- get_appopt(blocking_data, block_size, TRUE,  "lexical",         "adjacent_search", FALSE)
blockings$appopt_ULAV <- get_appopt(blocking_data, block_size, FALSE, "lexical",         "adjacent_search", FALSE)
blockings$appopt_D2AV <- get_appopt(blocking_data, block_size, TRUE,  "2ndPowOrder",     "adjacent_search", FALSE)
blockings$appopt_U2AV <- get_appopt(blocking_data, block_size, FALSE, "2ndPowOrder",     "adjacent_search", FALSE)
blockings$appopt_DHAV <- get_appopt(blocking_data, block_size, TRUE,  "heuristicSearch", "adjacent_search", FALSE)
blockings$appopt_UHAV <- get_appopt(blocking_data, block_size, FALSE, "heuristicSearch", "adjacent_search", FALSE)

if (!file.exists(outfile)) {
  save(raw_data, blockings, batch_name, block_size, block_on, file = outfile, compress = FALSE)
  invisible(file.remove(batch_torun))
}
