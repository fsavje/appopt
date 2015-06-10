library("reshape2")

allruns <- lapply(list.files("results"), function(resfile) {
  cbind(read.delim(paste0("results/", resfile),
                   header = FALSE, colClasses = "character",
                   sep = " ", quote = "", stringsAsFactors = FALSE),
        resfile, stringsAsFactors = FALSE)
})

allruns <- do.call(rbind, allruns)
names(allruns) <- c("method", "sample_size", "covs", "round", "sep", "sys", "user", "mem", "fromsim")

allruns$mem <- as.numeric(allruns$mem)
allruns$cpu <- as.numeric(allruns$sys) + as.numeric(allruns$user)
allruns$sep <- allruns$sys <- allruns$user <- NULL
allruns$round <- as.integer(allruns$round)

cpu_res <- dcast(allruns, sample_size + covs + round + fromsim ~ method, value.var = "cpu", fill = NA)
mem_res <- dcast(allruns, sample_size + covs + round + fromsim ~ method, value.var = "mem", fill = NA)

cpu_res$fromsim <- mem_res$fromsim <- NULL

# Some runs got aborted, these were ran again complete from the start
# Remove 60 incomplete runs:
cpu_res$ss_int <- as.integer(cpu_res$sample_size)
mem_res$ss_int <- as.integer(mem_res$sample_size)
sum(is.na(cpu_res$aoD2SG) | (is.na(cpu_res$greedy) & cpu_res$ss_int <= 20000))
sum(is.na(mem_res$aoD2SG) | (is.na(mem_res$greedy) & mem_res$ss_int <= 20000))
cpu_res <- cpu_res[!(is.na(cpu_res$aoD2SG) | (is.na(cpu_res$greedy) & cpu_res$ss_int <= 20000)), ]
mem_res <- mem_res[!(is.na(mem_res$aoD2SG) | (is.na(mem_res$greedy) & mem_res$ss_int <= 20000)), ]
cpu_res$ss_int <- mem_res$ss_int <- NULL

# Check so 250 rounds for each setting
check_cpu <- factor(paste(cpu_res$sample_size, cpu_res$covs, sep = "-"))
for(i in unique(check_cpu)) {
  stopifnot(length(cpu_res$round[check_cpu == i]) == 250,
            all(1L:250L %in% cpu_res$round[check_cpu == i]))
}
check_mem <- factor(paste(mem_res$sample_size, mem_res$covs, sep = "-"))
for(i in unique(check_mem)) {
  stopifnot(length(mem_res$round[check_mem == i]) == 250,
            all(1L:250L %in% mem_res$round[check_mem == i]))
}

# Average over rounds
cpu_res <- recast(cpu_res, sample_size + covs ~ variable,
                  id.var = c("sample_size", "covs", "round"),
                  fun.aggregate = mean)
mem_res <- recast(mem_res, sample_size + covs ~ variable,
                  id.var = c("sample_size", "covs", "round"),
                  fun.aggregate = mean)

cpu_res <- rbind(cpu_res, list("1e8", "10", NA, NA, NA, NA, NA, NA, NA, NA))
mem_res <- rbind(mem_res, list("1e8", "10", NA, NA, NA, NA, NA, NA, NA, NA))

cpu_res$sample_size <- as.integer(cpu_res$sample_size)
mem_res$sample_size <- as.integer(mem_res$sample_size)
cpu_res$covs <- as.integer(cpu_res$covs)
mem_res$covs <- as.integer(mem_res$covs)
       
save(cpu_res, mem_res, file = "results.RData")
