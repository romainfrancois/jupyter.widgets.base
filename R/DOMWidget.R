#' @include Widget.R
#' @include Style.R
#' @include Layout.R
NULL

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
#' @return a [jupyter.widget.DOMWidget] object
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

#' a DOM Widget
#'
#' @export
jupyter.widget.DOMWidget <- R6Class("jupyter.widget.DOMWidget",
  inherit = jupyter.widget.Widget,

  public = list(

    #' @param layout a [Layout()]
    #' @param style a [Style()]
    #' @param tabbable is the widget tabbable
    #' @param tooltip tooltip
    #' @param _dom_classes CSS classes applied to widget DOM element
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

    #' @title generate the mime bundle for the widget
    #' @return the mime bundle for the widget
    #' @export
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

    #' @field layout
    #' the [Layout()] for this widget
    layout = function() layout_,

    #' @field style
    #' the [Style()] for this widget
    style  = function() style_,

    #' @field tabbable
    #' Is this widget tabbable. Read/Write
    tabbable = function(x) if (missing(x)) private$state_[["tabbable"]] else self$update(tabbable = unbox(x)),

    #' @field tooltip
    #' tooltip. Read/Write
    tooltip  = function(x) if (missing(x)) private$state_[["tooltip"]] else self$update(tooltip = x),

    #' @field _dom_classes
    #' DOM classes. Read only
    `_dom_classes` = function() private$state_[["_dom_classes"]]
  ),

  private = list(
    layout_ = NULL,
    style_  = NULL
  )
)
