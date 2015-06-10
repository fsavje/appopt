envir <- file("envir.sh", "r")
envir_lines <- readLines(envir)
close(envir)
envir_lines <- envir_lines[grep("^SCRATCHDIR=", envir_lines)[1]]
scratch_dir <- unlist(strsplit(envir_lines, "\"", fixed = TRUE))[2]

if (!file.exists(substr(scratch_dir, 1, nchar(scratch_dir) - 1))) {
  warning("Invalid scratch dir.")
  quit("no", 1)
}

write_batches <- function(to_run, samples, covs, folder) {
  invisible(lapply(samples, function(ss) {
    lapply(covs, function(cs) {
      lapply(1L:n_rounds, function(r) {
        write_to_file <- file(paste0(folder, "/", ss, "-", cs, "-", r), "w")
        cat(paste0("BATCHSET=\"", ss, " ", cs, " ", r, "\""),
            paste0("TORUN=\"", to_run, "\""),
            file = write_to_file, sep = "\n")
        close(write_to_file)
      })
    })
  }))
}

if (!file.exists(paste0(scratch_dir, "b4G"))) dir.create(paste0(scratch_dir, "b4G"))
if (!file.exists(paste0(scratch_dir, "b6G"))) dir.create(paste0(scratch_dir, "b6G"))
if (!file.exists(paste0(scratch_dir, "b25G"))) dir.create(paste0(scratch_dir, "b25G"))
if (!file.exists(paste0(scratch_dir, "b32G"))) dir.create(paste0(scratch_dir, "b32G"))
if (!file.exists(paste0(scratch_dir, "tmpdata"))) dir.create(paste0(scratch_dir, "tmpdata"))
if (!file.exists(paste0(scratch_dir, "running"))) dir.create(paste0(scratch_dir, "running"))

set.seed(19860604)

# Table
sample_sizes <- c("1e2", "1e3", "1e4", "2e4", "5e4", "1e5", "1e6", "1e7", "1e8")

# Small graph
sample_sizes <- c(sample_sizes, "1e1", "25e2", "50e2", "75e2", "125e2", "150e2", "175e2", "225e2",
                  "250e2", "275e2", "300e2", "325e2", "350e2", "375e2", "400e2")

# Large graph
sample_sizes <- c(sample_sizes, "2e7", "3e7", "4e7", "5e7", "6e7", "7e7", "8e7", "9e7")

# Log graph
sample_sizes <- c(sample_sizes, "18e1", "32e1", "56e1", "18e2", "32e2", "56e2", 
                  "18e3", "32e3", "56e3", "18e4", "32e4", "56e4", 
                  "18e5", "32e5", "56e5", "18e6", "32e6", "56e6")

names(sample_sizes) <- sample_sizes
cov_sizes <- c("2", "5", "10")
names(cov_sizes) <- cov_sizes
n_rounds <- 250L
#n_rounds <- 10L

seeds <- lapply(sample_sizes, function(ss) {
  lapply(cov_sizes, function(cs) {
    lapply(1L:n_rounds, function(r) {
      as.integer(floor(runif(1) * 100000000))
    })
  })
})

save(seeds, file = paste0(scratch_dir, "seeds.RData"), compress = FALSE)

# Table
write_batches("all", c("1e2", "1e3"), cov_sizes, paste0(scratch_dir, "b4G"))
write_batches("all", c("1e4"), cov_sizes, paste0(scratch_dir, "b25G"))
write_batches("all", c("2e4"), cov_sizes, paste0(scratch_dir, "b32G"))
write_batches("appopt", c("5e4", "1e5", "1e6"), cov_sizes, paste0(scratch_dir, "b4G"))
write_batches("appopt", c("1e7"), cov_sizes, paste0(scratch_dir, "b6G"))
write_batches("appopt", c("1e8"), c("2", "5"), paste0(scratch_dir, "b32G"))

# Small graph
write_batches("all", c("1e1"), c("2"), paste0(scratch_dir, "b4G"))
write_batches("all", c("25e2", "50e2", "75e2"), c("2"), paste0(scratch_dir, "b25G"))
write_batches("all", c("125e2", "150e2", "175e2"), c("2"), paste0(scratch_dir, "b32G"))
write_batches("appopt", c("225e2", "250e2", "275e2", "300e2", "325e2", "350e2", "375e2", "400e2"), c("2"), paste0(scratch_dir, "b4G"))

# Large graph
write_batches("appopt", c("2e7", "3e7", "4e7", "5e7", "6e7", "7e7"), c("2"), paste0(scratch_dir, "b25G"))
write_batches("appopt", c("8e7", "9e7"), c("2"), paste0(scratch_dir, "b32G"))

# Log graph
write_batches("all", c("18e1", "32e1", "56e1"), c("2"), paste0(scratch_dir, "b4G"))
write_batches("all", c("18e2", "32e2", "56e2"), c("2"), paste0(scratch_dir, "b25G"))
write_batches("all", c("18e3"), c("2"), paste0(scratch_dir, "b32G"))
write_batches("appopt", c("32e3", "56e3", "18e4", "32e4", "56e4"), c("2"), paste0(scratch_dir, "b4G"))
write_batches("appopt", c("18e5", "32e5", "56e5"), c("2"), paste0(scratch_dir, "b6G"))
write_batches("appopt", c("18e6", "32e6", "56e6"), c("2"), paste0(scratch_dir, "b25G"))
