method_names <- c("unadj" = "Unadjusted",
                  "OLScov" = "OLS adjustment",
                  "moore" = "Fixed greedy",
                  "greedy" = "Threshold greedy",
                  "nbpmatch" = "Non-bipartite matching",
                  "appopt_D2SG" = "Improvements 1-4",
                  "appopt_D2AG" = "Improvements 1-3",
                  "appopt_DLAV" = "Directed version",
                  "appopt_ULAV" = "Approximation algorithm")

extract <- function(model, ss, bs, get_rows, get_cols) {
  load(paste0(folder, model, "-", ss, "-", bs, ".RData"))
  results[intersect(get_rows, rownames(results)), intersect(get_cols, colnames(results)), drop = FALSE]
}

get_table <- function(model, sample_sizes, bs, get_rows, get_cols, normalize_with) {
  t(do.call(rbind, lapply(sample_sizes, function(ss) {
    ret <- extract(model, ss, bs, get_rows, get_cols)
    rownames(ret) <- paste0(ss, "-", rownames(ret))
    if (!is.null(normalize_with)) {
      ret <- ret / ret[, normalize_with]
    }
    ret
  })))
}

get_text_mat <- function(res_mat, add_colnames, colsep, rowsep) {
  res_mat <- sub("NA", "  ", res_mat)
  res_mat <- cbind(rownames(res_mat), res_mat)
  if (add_colnames) {
    res_mat <- rbind(colnames(res_mat), res_mat)
  }
  res_mat <- apply(res_mat, 2, function(col) {
    format(col, justify = "right")
  })
  res_mat <- apply(res_mat, 1, paste, collapse = colsep)
  res_mat <- paste(res_mat, collapse = rowsep)
  res_mat
}

print_text <- function(res_mat) {
  cat(get_text_mat(res_mat, TRUE, "  ", "\n"))
}

save_latex <- function(res_mat, filename) {
  latex_file <- file(filename, "w")
  out <- paste0(get_text_mat(res_mat, FALSE, " & ", " \\\\ \n"), " \\\\ ")
  cat(out, file = latex_file, sep = "")
  close(latex_file)
}
