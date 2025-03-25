#' @include factory.R
NULL

#' Base class for jupyter widgets
#'
#' @rdname Widget
#' @export
jupyter.widget.Widget <- R6::R6Class("jupyter.widget.Widget",
  public = list(

    #' @param _model_module The namespace of the model.
    #' @param _model_module_version A semver requirement for namespace version containing the model.
    #' @param _model_name model name
    #' @param _view_module view mmodule
    #' @param _view_count view count
    #' @param _view_module_version view module version
    #' @param _view_name view name
    #'
    #' @param ... unused
    #' @param error_call see [rlang::args_error_context()]
    #'
    #' @return a new `jupyter.widget.Widget` object
    initialize = function(
      `_model_module` = '@jupyter-widgets/base',
      `_model_module_version` = "2.0.0",
      `_model_name` = "",
      `_view_module` = '@jupyter-widgets/base',
      `_view_count` = NULL,
      `_view_module_version` = "2.0.0",
      `_view_name` = "",
      ...,
      error_call = caller_env()
    ) {

      private$state_ <- update_list(private$state_,
        `_model_module` = unbox(`_model_module`),
        `_model_module_version` = unbox(`_model_module_version`),
        `_model_name` = unbox(`_model_name`),
        `_view_module` = unbox(`_view_module`),
        `_view_count` = unbox(`_view_count`),
        `_view_module_version` = unbox(`_view_module_version`),
        `_view_name` = unbox(`_view_name`)
      )

      rlang::check_dots_empty(call = error_call)

      private$handlers_ <- new.env()
      private$check_state_env_ <- new.env()
      private$comm_ <- comm <- CommManager$new_comm("jupyter.widget")

      comm$on_message(function(request) {
        data <- request$content$data
        method <- data$method

        switch(
          method,
          update = {
            state <- data$state
            private$state_ <- replace(private$state_, names(state), state)
            private$handle("update", state)

            comm$send(
              data = list(
                method = "echo_update", state = state, buffer_paths = list()
              )
            )
          },

          custom = {
            private$handle("custom", data$content)
          })
      })

      comm$on_close(function(request) {
        private$handle("on_close", data$content)
      })

      data <- list(state = private$state_, buffer_paths = list())
      if (isTRUE(getOption("comm.verbose"))) {
        print(jsonlite::prettify(jsonlite::toJSON(data)))
      }
      private$before_comm_open()
      comm$open(
        data = data,
        metadata = list(version = "2.1.0")
      )
      private$after_comm_open()

    },

    #' get a state
    #'
    #' @param name name of the state to get
    #' @return the current value of the state
    state = function(name) {
      private$state_[[name]]
    },

    #' check a state
    #'
    #' @param name name
    #' @param value value
    #'
    #' @return a value suitable for a state
    check_state = function(name, value) {
      fun <- private$check_state_env_[[name]]
      if (!is.null(fun)) {
        fun(value)
      } else {
        unbox(value)
      }
    },

    #' Set a state checker
    #'
    #' @param name name of the state
    #' @param fun function that takes a single parameter, checks it and returns something that is suitable for a state
    set_state_check = function(name, fun) {
      private$check_state_env_[[name]] <- fun
    },

    #' update states
    #'
    #' update state in the Widget object and send a comm
    #' message to update the state in the front end too
    #'
    #' @param ... states
    update = function(...) {
      state <- list2(...)

      private$comm_$send(
        data = list(method = "update", state = state, buffer_paths = list())
      )
    },

    #' Setup a handler to handle "update" messages from the front end
    #' @param handler handler function
    on_update = function(handler) {
      private$handlers_[["update"]] <- handler
    },

    #' Setup a handler to handle "update" messages from the front end
    #' @param handler handler function
    on_custom = function(handler) {
      private$handlers_[["custom"]] <- handler
    }
  ),

  private = list(
    state_ = list(),
    comm_ = NULL,
    handlers_ = NULL,
    check_state_env_ = NULL,

    handle = function(name, ...) {
      handler <- private$handlers_[[name]]
      if (!is.null(handler)) {
        handler(...)
      }
    },

    before_comm_open = function(){},
    after_comm_open = function(){}
  ),

  active = list(

    #' @field model_id
    #' the model id, i.e. the id of the associated comm object
    model_id = function() private$comm_$id,

    #' @field comm
    #' the widget comm
    comm = function() private$comm_,

    #' @field _model_module
    #' the model module
    `_model_module`         = function() private$state_[["_model_module"]],

    #' @field _model_module_version
    #' the model module version
    `_model_module_version` = function() private$state_[["_model_module_version"]],

    #' @field _model_name
    #' the model name
    `_model_name`           = function() private$state_[["_model_name"]],

    #' @field _view_module
    #' the view module
    `_view_module`          = function() private$state_[["_view_module"]],

    #' @field _view_count
    #' the view count
    `_view_count`           = function() private$state_[["_view_count"]],

    #' @field _view_module_version
    #' the view module version
    `_view_module_version`  = function() private$state_[["_view_module_version"]],

    #' @field _view_name
    #' the view name
    `_view_name`            = function() private$state_[["_view_name"]]
  )
)

#' Widget
#'
#' @param ... See constructor for `jupyter.widgets.Widget`
#' @inheritParams rlang::args_error_context
#'
#' @return a [jupyter.widget.Widget] object
#'
#' @export
Widget <- factory(jupyter.widgets.Widget)
