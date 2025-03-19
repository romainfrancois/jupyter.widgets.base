jupyter.widget.Style <- R6Class("jupyter.widget.Style", inherit = jupyter.widget.Widget,
  public = list(
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
#' @inheritParams Widget
#'
#' @export
Style <- function(
  # Widget
  `_model_module` = '@jupyter-widgets/base',
  `_model_module_version` = "2.0.0",
  `_model_name` = "",
  `_view_module` = '@jupyter-widgets/base',
  `_view_count` = NULL,
  `_view_module_version` = "2.0.0",
  `_view_name` = "",

  # Widget
  ...,
  error_call = caller_env()
){
  jupyter.widget.Style$new(
    `_model_module` = `_model_module`,
    `_model_module_version` = `_model_module_version`,
    `_model_name` = `_model_name`,
    `_view_module` = `_view_module`,
    `_view_count` = `_view_count`,
    `_view_module_version` = `_view_module_version`,
    `_view_name` = `_view_name`,
    ...,
    error_call = error_call
  )
}
