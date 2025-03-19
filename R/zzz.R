#' @import R6
#' @import hera
#' @import assertthat
#' @import jsonlite
#' @importFrom rlang current_env check_dots_empty caller_env arg_match is_true list2 caller_arg
#' @importFrom glue glue
#' @importFrom cli cli_abort
NULL

handler_jupyter.widget.control <- function(comm, message) {

  comm$on_message(function(request) {
    data <- request$content$data

    switch(data$method,
      "request_states" = {
        comm$send(
          data = list(
            method = unbox("update_states"),
            state = NULL
          )
        )
      }
    )
  })
}

handler_jupyter.widget <- function(comm, message) {
    comm$on_message(function(request) {

    })
}

.onLoad <- function(libname, pkgname) {
  if (is_xeusr()) {
    CommManager$register_comm_target("jupyter.widget", handler_jupyter.widget)
  }
}
