
source("tableconst.R")

folder <- "statresults/"

get_stats <- function(bs, get_methods) {
  mxd <- get_table("ad", sample_sizes, bs, "max_dist", get_methods, "appopt_ULAV")
  med <- get_table("ad", sample_sizes, bs, "mean_dist", get_methods, "appopt_ULAV")
  meb <- get_table("ad", sample_sizes, bs, "mean_bl", get_methods, NULL)
  statres <- cbind(format(mxd, digits = 3, nsmall = 1),
                   " ", format(med, digits = 3, nsmall = 1),
                   " ", format(meb, digits = 3, nsmall = 1))
  rownames(statres) <- method_names[rownames(statres)]
  statres
}

#sample_sizes <- c("100", "500", "1k", "5k", "10k")
sample_sizes <- c("100", "1k", "10k")
get_methods_all <- c("appopt_ULAV", "appopt_DLAV", "appopt_D2AG", "appopt_D2SG", "moore", "greedy", "nbpmatch")
get_methods <-     c("appopt_ULAV", "appopt_DLAV", "appopt_D2AG",                "moore", "greedy", "nbpmatch")

statres2 <- get_stats(2, get_methods)
statres4 <- get_stats(4, get_methods_all)

print_text(statres2)
print_text(statres4)

save_latex(statres2, "latex/bl_stats2.tex")
save_latex(statres4, "latex/bl_stats4.tex")
