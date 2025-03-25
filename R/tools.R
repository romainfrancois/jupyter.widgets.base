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

#' @inheritParams rlang::arg_match
#' @rdname tools
#' @export
arg_match_or_null <- function(arg, values = NULL, ..., multiple = FALSE, error_arg = caller_arg(arg), error_call = caller_env()) {
  if (is.null(arg)) NULL else {
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

namedlist <- function() {
  `names<-`(list(), character())
}

#' Create a state check function for a given set of accepted values
#'
#' @param values accepted values
#' @param allow_empty TRUE if "" is accepted
#' @param allow_null TRUE if NULL is accepted
#'
#' @export
unbox_one_of <- function(values, allow_empty = FALSE, allow_null = FALSE) {
  matcher <- if (allow_empty) {
    jupyter.widgets.base::arg_match_or_empty
  } else if (allow_null) {
    jupyter.widgets.base::arg_match_or_null
  } else {
    rlang::arg_match
  }

  function(value) {
    unbox(matcher(value, values = values))
  }
}
