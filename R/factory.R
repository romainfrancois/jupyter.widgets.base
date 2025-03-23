#' Factory function for R6 jupyter classes
#'
#' @param class R6 class object
#'
#' @return a function that takes `...` and `error_call` and forwards
#' them to the constructor of the class.
#'
#' @export
factory <- function(class) {
  function(..., error_call = current_env()) {
    class$new(..., error_call = error_call)
  }
}

