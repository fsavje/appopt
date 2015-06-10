sim_torun <- commandArgs(trailingOnly = TRUE)[1]

sim_name <- unlist(strsplit(sim_torun, "/", fixed = TRUE))
sim_name <- sim_name[length(sim_name)]
outfile <- paste0("estresults/", sim_name)

if (!file.exists("estresults")) dir.create("estresults")

stopifnot(file.exists(sim_torun),
          !file.exists(outfile))

set.seed(19860604)

suppressPackageStartupMessages(library("appopt"))

load(sim_torun)

get_batches <- function(method) {
  lapply(ready, function(batch) {
    batch <- data.frame(batch$raw_data, block = batch$blockings[[method]])
    batch <- data.frame(batch, treatment = get_randomize(batch$block, 0L:1L))
    batch$y <- batch$y1
    batch$y[batch$treatment == 0] <- batch$y0[batch$treatment == 0]
    batch$y1 <- batch$y0 <- NULL
    batch
  })
}

est_DIM <- function(batches) {
  sapply(batches, function(df) {
    mean(df$y[df$treatment == 1L]) - mean(df$y[df$treatment == 0L])
  })
}

est_OLSadj <- function(batches) {
  fml_cov <- paste("y ~ treatment", paste(settings$block_on, collapse = " + "), sep = " + ")
  sapply(batches, function(df) {
    lm(formula(fml_cov), data = df)$coefficients["treatment"]
  })
}

est_blockDIM <- function(batches) {
  sapply(batches, function(df) {
    get_estimate(df$y, df$block, df$treatment, 1L:0L)
  })
}

get_bootstr <- function(rounds, statistic) {
  sd(sapply(1:1000, function(r) {
    statistic(rounds[sample(length(rounds), replace = TRUE)])
  }))
}

get_stats <- function(ests) {
  g_bias <- function(ests) abs(mean(ests - true_TE))
  g_rmse <- function(ests) sqrt(mean((ests - true_TE)^2))
  
  c(bias = g_bias(ests),
    bias_mcsd = get_bootstr(ests, g_bias),
    rmse = g_rmse(ests),
    rmse_mcsd = get_bootstr(ests, g_rmse),
    sd = sd(ests),
    sd_mcsd = get_bootstr(ests, sd))
}


non_dry <- setdiff(names(ready[[1]]$blockings), "dry")
names(non_dry) <- non_dry

# Replicate each batch 4 times (to 4 * 5,000 = batches) so to minimize
# noise from random assignment of treatment.
ready <- rep(ready, 4)

true_TE <- sapply(ready, function(batch) {
  mean(batch$raw_data$y1 - batch$raw_data$y0)
})

tmp_dry <- get_batches("dry")
results <- cbind(unadj = get_stats(est_DIM(tmp_dry)),
                 OLScov = get_stats(est_OLSadj(tmp_dry)))

results <- cbind(results,
                 sapply(non_dry, function(method) {
                   get_stats(est_blockDIM(get_batches(method)))
                 }))

if (!file.exists(outfile)) {
  save(results, settings, file = outfile)
  invisible(file.remove(sim_torun))
}
