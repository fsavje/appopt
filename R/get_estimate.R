#' @useDynLib appopt cpp_estimate

#' @export
get_estimate <- function(outcomes, blocking, treatments, contrast) {

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

  blocking <- blocking - 1L
  .Call("cpp_estimate", outcomes, blocking, treatments, contrast, PACKAGE = "appopt")

}
