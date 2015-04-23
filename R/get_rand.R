#' @useDynLib appopt cpp_randomize

#' @export
get_randomize <- function(blocking, treat_ind) {

  stopifnot(is.vector(blocking),
            is.integer(blocking),
            any(!is.na(blocking)),
            length(blocking) >= 2,
            is.vector(treat_ind),
            is.integer(treat_ind),
            any(!is.na(treat_ind)),
            length(treat_ind) >= 2)

  blocking <- blocking - 1L
  .Call("cpp_randomize", blocking, treat_ind, PACKAGE = "appopt")
}

