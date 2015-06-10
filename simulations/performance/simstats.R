sim_torun <- commandArgs(trailingOnly = TRUE)[1]

sim_name <- unlist(strsplit(sim_torun, "/", fixed = TRUE))
sim_name <- sim_name[length(sim_name)]
outfile <- paste0("statresults/", sim_name)

if (!file.exists("statresults")) dir.create("statresults")

stopifnot(file.exists(sim_torun),
          !file.exists(outfile))

suppressPackageStartupMessages(library("appopt"))

load(sim_torun)

get_bl_dist <- function(blocking, norm_cov) {
  bl_table <- table(blocking)
  output <- unlist(get_block_stats(norm_cov, settings$block_size, blocking))
  c(output, max_bl = max(bl_table), mean_bl = mean(bl_table))
}

extract_mat <- function(bl_dist, what) {
  t(sapply(bl_dist, function(b) {
    sapply(b, function(m) { unname(m[what]) })
  }))
}

boot_mean <- function(rounds) {
  sd(sapply(1:1000, function(r) {
    mean(rounds[sample(length(rounds), replace = TRUE)])
  }))
}


bl_dist <- lapply(ready, function(batch) {
  lapply(batch$blockings,
         get_bl_dist,
         normalize_cov(batch$raw_data[settings$block_on]))
})
max_dist <- extract_mat(bl_dist, "max")
mean_dist <- extract_mat(bl_dist, "mean")
max_bl <- extract_mat(bl_dist, "max_bl")
mean_bl <- extract_mat(bl_dist, "mean_bl")

results <- rbind(max_dist = apply(max_dist, 2, mean),
                 max_dist_mcsd = apply(max_dist, 2, boot_mean),
                 mean_dist = apply(mean_dist, 2, mean),
                 mean_dist_mcsd = apply(mean_dist, 2, boot_mean),
                 max_bl = apply(max_bl, 2, mean),
                 max_bl_mcsd = apply(max_bl, 2, boot_mean),
                 mean_bl = apply(mean_bl, 2, mean),
                 mean_bl_mcsd = apply(mean_bl, 2, boot_mean))

if (!file.exists(outfile)) {
  save(results, settings, file = outfile)
  invisible(file.remove(sim_torun))
}
