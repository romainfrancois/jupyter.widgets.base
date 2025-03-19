jupyter.widget.DOMWidget <- R6Class("jupyter.widget.DOMWidget",
  inherit = jupyter.widget.Widget,

  public = list(
    initialize = function(
      # DOMWidget
      layout = Layout(),
      style = NULL,
      tabbable = FALSE,
      tooltip = "",
      `_dom_classes` = character(),

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
      private$layout_ <- layout
      private$style_  <- style

      private$state_ <- update_list(private$state_,
        tabbable = unbox(isTRUE(tabbable)),
        tooltip  = unbox(ensure(tooltip, null_or(is.string))),
        layout   = unbox(glue("IPY_MODEL_{layout$comm$id}")),
        `_dom_classes` = `_dom_classes`
      )

      if (!is.null(style)) {
        private$state_$style <- unbox(glue("IPY_MODEL_{style$comm$id}"))
      }

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
    },

    mime_bundle = function() {
      data <- list(
        "application/vnd.jupyter.widget-view+json" = list(
          "version_major" = unbox(2L),
          "version_minor" = unbox(0L),
          "model_id"      = unbox(self$comm$id)
        )
      )
      list(data = data, metadata = namedlist())
    }
  ),

  active = list(
    layout = function() layout_,
    style  = function() style_,

    tabbable = function(x) if (missing(x)) private$state_[["tabbable"]] else self$update(tabbable = x),
    tooltip  = function(x) if (missing(x)) private$state_[["tooltip"]] else self$update(tooltip = x),

    `_dom_classes` = function() private$state_[["_dom_classes"]]
  ),

  private = list(
    layout_ = NULL,
    style_  = NULL
  )
)

#' A DOM Widget
#'
#' @param layout a [Layout()]
#' @param style a [Style()]
#'
#' @param tabbable is the widget tabbable
#' @param tooltip tooltip
#' @param _dom_classes CSS classes applied to widget DOM element
#'
#' @inheritParams Widget
#'
#' @export
DOMWidget <- function(
    # DOMWidget
    layout = Layout(),
    style = NULL,
    tabbable = FALSE,
    tooltip = "",
    `_dom_classes` = character(),

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
  jupyter.widget.DOMWidget$new(
    # DOMWidget
    layout = layout,
    style = style,
    tabbable = tabbable,
    tooltip = tooltip,
    `_dom_classes` = `_dom_classes`,

    # Widget
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
