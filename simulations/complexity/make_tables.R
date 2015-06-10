load("results.RData")

ss_table <- c(1e2, 1e3, 1e4, 2e4, 5e4, 1e5, 1e6, 1e7, 1e8)

methods  <-   c("aoULAV" = "Approximation algorithm",
                "aoDLAV"= "Directed version",
                "aoD2AG" = "Improvements 1-3",
                "aoD2SG" = "Improvements 1-4",
                "moore" = "Fixed greedy",
                "greedy" = "Threshold greedy",
                "nbpm" = "Non-bipartite matching")

cpu_res <- cpu_res[cpu_res$sample_size %in% ss_table, ]
mem_res <- mem_res[mem_res$sample_size %in% ss_table, ]


get_cpu <- function(dimensions) {
  tmp_mat <- cpu_res[cpu_res$covs == dimensions, ]
  tmp_mat <- t(as.matrix(tmp_mat[order(tmp_mat$sample_size), names(methods)]))
  rownames(tmp_mat) <- methods
  format(tmp_mat, digits = 1, nsmall = 1, scientific = FALSE, big.mark = ",")
}

get_mem <- function(dimensions) {
  tmp_mat <- mem_res[mem_res$covs == dimensions, ]
  tmp_mat <- t(as.matrix(tmp_mat[order(tmp_mat$sample_size), names(methods)]))
  tmp_mat <- tmp_mat / 1024
  rownames(tmp_mat) <- methods
  format(tmp_mat, digits = 1, nsmall = 0, scientific = FALSE, big.mark = ",")
}

get_text_mat <- function(res_mat, colsep, rowsep) {
  res_mat <- sub("NA", "  ", res_mat)
  res_mat <- cbind(rownames(res_mat), res_mat)
  res_mat <- apply(res_mat, 2, function(col) {
    format(col, justify = "right")
  })
  res_mat <- apply(res_mat, 1, paste, collapse = colsep)
  res_mat <- paste(res_mat, collapse = rowsep)
  res_mat
}

save_latex <- function(res_mat, filename) {
  latex_file <- file(filename, "w")
  out <- paste0(get_text_mat(res_mat, " & ", " \\\\ \n"), " \\\\ ")
  cat(out, file = latex_file, sep = "")
  close(latex_file)
}


save_latex(get_cpu(2), "latex/cpu_2.tex")
save_latex(get_cpu(5), "latex/cpu_5.tex")
save_latex(get_cpu(10), "latex/cpu_10.tex")

save_latex(get_mem(2), "latex/mem_2.tex")
save_latex(get_mem(5), "latex/mem_5.tex")
save_latex(get_mem(10), "latex/mem_10.tex")
