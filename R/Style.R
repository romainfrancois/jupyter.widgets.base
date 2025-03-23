#' @include Widget.R
NULL

#' Style class
#'
#' @rdname Style
#' @export
jupyter.widget.Style <- R6Class("jupyter.widget.Style", inherit = jupyter.widget.Widget,
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
    initialize = function(
      # Widget
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
      super$initialize(
        # Widget
        `_model_module` = unbox(`_model_module`),
        `_model_module_version` = unbox(`_model_module_version`),
        `_model_name` = unbox(`_model_name`),
        `_view_module` = unbox(`_view_module`),
        `_view_count` = `_view_count`,
        `_view_module_version` = unbox(`_view_module_version`),
        `_view_name` = unbox(`_view_name`),

        ...,
        error_call = error_call
      )
    }
  )
)

#' Style
#'
#' @param ... See constructor for `jupyter.widgets.Style`
#' @inheritParams rlang::args_error_context
#'
#' @export
Style <- factory(jupyter.widgets.Style)
