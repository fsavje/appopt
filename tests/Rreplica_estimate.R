Rrep_estimate <- function(outcomes, blocking, treatments, contrast) {

  stopifnot(is.vector(outcomes),
            is.numeric(outcomes),
            any(!is.na(outcomes)),
            is.vector(blocking),
            is.integer(blocking),
            any(!is.na(blocking)),
            length(blocking) == length(outcomes),
            is.vector(treatments),
            is.integer(treatments),
            any(!is.na(treatments)),
            length(treatments) == length(outcomes),
            is.vector(contrast),
            is.integer(contrast),
            any(!is.na(contrast)),
            length(contrast) == 2)


  estimate <- sum(sapply(unique(blocking), function (b) {
    in_block <- (blocking == b)
    if (sum(in_block & treatments == contrast[1]) == 0 ||
          sum(in_block & treatments == contrast[2]) == 0) {
      stop("Some treatments are missing in a block.")
    }

    sum(in_block) * (mean(outcomes[in_block & treatments == contrast[1]]) - mean(outcomes[in_block & treatments == contrast[2]]))
  }))

  estimate / length(outcomes)
}

