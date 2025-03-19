#' update list
#'
#' @param x list to update
#' @param ... new items
#'
#' @rdname tools
#' @export
update_list <- function(x, ...) {
  dots <- rlang::list2(...)
  x[names(dots)] <- dots
  x
}

#' @param default default value
#' @param fun function
#'
#' @rdname tools
#' @export
default_or <- function(default, fun) {
  function(x) {
    identical(x, default) || fun(x)
  }
}

#' @rdname tools
#' @export
null_or <- function(fun) {
  function(x) {
    is.null(x) || fun(x)
  }
}

#' @inheritParams rlang::arg_match
#' @rdname tools
#' @export
arg_match_or_empty <- function(arg, values = NULL, ..., multiple = FALSE, error_arg = caller_arg(arg), error_call = caller_env()) {
  if (identical(arg, "")) "" else {
    rlang::arg_match(arg, values = values, ..., error_arg = error_arg, error_call = error_call)
  }
}

#' @inheritParams assertthat::assert_that
#' @rdname tools
#' @export
ensure <- function(x, fun = assertthat::is.string, ..., msg = NULL) {
  assert_that(fun(x, ...), msg = msg)

  x
}
