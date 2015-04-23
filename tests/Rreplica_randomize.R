Rrep_sample <- function(x, to_draw) {
  out <- rep(NA, to_draw)
  for (i in 1:to_draw) {
    swap_with <- floor(i + runif(1) * (length(x) - i + 1))
    out[i] <- x[swap_with]
    x[swap_with] <- x[i]
  }
  out
}

Rrep_shuffle <- function(x) Rrep_sample(x, length(x))

Rrep_randomize <- function(blocking, treat_ind) {

  stopifnot(is.vector(blocking),
            is.integer(blocking),
            any(!is.na(blocking)),
            length(blocking) >= 2,
            is.vector(treat_ind),
            is.integer(treat_ind),
            any(!is.na(treat_ind)),
            length(treat_ind) >= 2)

  treatment <- rep(NA, length(blocking))

  for (b in 1:max(blocking)) {
    n_in_block <- sum(blocking == b)

    tmp_treat <- NULL
    if (n_in_block %% length(treat_ind) != 0) {
      tmp_treat <- Rrep_sample(treat_ind, n_in_block %% length(treat_ind))
    }
    tmp_treat <- c(tmp_treat, rep(treat_ind, n_in_block %/% length(treat_ind)))
    tmp_treat <- rev(Rrep_shuffle(tmp_treat))

    treatment[blocking == b] <- tmp_treat
  }

  treatment
}
