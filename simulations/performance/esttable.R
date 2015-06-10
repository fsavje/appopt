
source("tableconst.R")

folder <- "estresults/"

get_ests <- function(bs, which_est, get_methods) {
  estres <- get_table("est-ad", sample_sizes, bs, which_est, get_methods, "appopt_ULAV")
  estres <- format(estres, digits = 3, nsmall = 1)
  rownames(estres) <- method_names[rownames(estres)]
  estres
}


#sample_sizes <- c("100", "500", "1k", "5k", "10k")
sample_sizes <- c("100", "1k", "10k")
get_methods_all <- c("appopt_ULAV", "appopt_DLAV", "appopt_D2AG", "appopt_D2SG", "moore", "greedy", "nbpmatch", "unadj", "OLScov")
get_methods <-     c("appopt_ULAV", "appopt_DLAV", "appopt_D2AG",                "moore", "greedy", "nbpmatch", "unadj", "OLScov")

estres2 <- get_ests(2, "rmse", get_methods)
estres4 <- get_ests(4, "rmse", get_methods_all)

print_text(estres2)
print_text(estres4)

save_latex(estres2, "latex/est2.tex")
save_latex(estres4, "latex/est4.tex")

