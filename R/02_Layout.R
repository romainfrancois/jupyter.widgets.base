#' Layout
#'
#' @param ... See [jupyter.widget.Layout$initialize]
#' @inheritParams rlang::args_error_context
#'
#' @return a new `jupyter.widget.Layout` object
#'
#' @export
Layout <- function(..., error_call = current_env()) {
  jupyter.widget.Layout$new(..., error_call = error_call)
}

#' Layout class
#'
#' @export
jupyter.widget.Layout <- R6::R6Class("jupyter.widget.Layout", inherit = jupyter.widget.Widget,
    public = list(

      #' @param align_content The align-content CSS attribute.
      #' @param align_items The align-items CSS attribute.
      #' @param align_self The align-self CSS attribute.
      #' @param border_bottom The border bottom CSS attribute.
      #' @param border_left The border left CSS attribute.
      #' @param border_right The border right CSS attribute.
      #' @param border_top The border top CSS attribute.
      #' @param bottom The bottom CSS attribute.
      #' @param display The display CSS attribute.
      #' @param flex The flex CSS attribute.
      #' @param flex_flow The flex-flow CSS attribute.
      #' @param grid_area The grid-area CSS attribute.
      #' @param grid_auto_columns The grid-auto-columns CSS attribute.
      #' @param grid_auto_flow The grid-auto-flow CSS attribute.
      #' @param grid_auto_rows The grid-auto-rows CSS attribute.
      #' @param grid_column The grid-column CSS attribute.
      #' @param grid_gap The grid-gap CSS attribute.
      #' @param grid_row The grid-row CSS attribute.
      #' @param grid_template_areas The grid-template-areas CSS attribute.
      #' @param grid_template_columns The grid-template-columns CSS attribute.
      #' @param grid_template_rows The grid-template-rows CSS attribute.
      #' @param height The height CSS attribute.
      #' @param justify_content The justify-content CSS attribute.
      #' @param justify_items The justify-items CSS attribute.
      #' @param left The left CSS attribute.
      #' @param margin The margin CSS attribute.
      #' @param max_height The max-height CSS attribute.
      #' @param max_width The max-width CSS attribute.
      #' @param min_height The min-height CSS attribute.
      #' @param min_width The min-width CSS attribute.
      #' @param object_fit The object-fit CSS attribute.
      #' @param object_position The object-position CSS attribute.
      #' @param order The order CSS attribute.
      #' @param overflow The overflow CSS attribute.
      #' @param padding The padding CSS attribute.
      #' @param right The right CSS attribute.
      #' @param top The top CSS attribute.
      #' @param visibility The visibility CSS attribute.
      #' @param width The width CSS attribute.
      #' @param ... unused
      #' @param error_call See [rlang::args_error_context()]
      initialize = function(
        # Layout
        align_content = NULL,
        align_items = NULL,
        align_self = NULL,
        border_bottom = NULL,
        border_left = NULL,
        border_right = NULL,
        border_top = NULL,
        bottom = NULL,
        display = NULL,
        flex = NULL,
        flex_flow = NULL,
        grid_area = NULL,
        grid_auto_columns = NULL,
        grid_auto_flow = NULL,
        grid_auto_rows = NULL,
        grid_column = NULL,
        grid_gap = NULL,
        grid_row = NULL,
        grid_template_areas = NULL,
        grid_template_columns = NULL,
        grid_template_rows = NULL,
        height = NULL,
        justify_content = NULL,
        justify_items = NULL,
        left = NULL,
        margin = NULL,
        max_height = NULL,
        max_width = NULL,
        min_height = NULL,
        min_width = NULL,
        object_fit = NULL,
        object_position = NULL,
        order = NULL,
        overflow = NULL,
        padding = NULL,
        right = NULL,
        top = NULL,
        visibility = NULL,
        width = NULL,
        ...,
        error_call = caller_env()
      ) {

        private$state_ <- update_list(private$state_,
          align_content = unbox(ensure(align_content, null_or(is.string))),
          align_items = unbox(ensure(align_items, null_or(is.string))),
          align_self = unbox(ensure(align_self, null_or(is.string))),
          border_bottom = unbox(ensure(border_bottom, null_or(is.string))),
          border_left = unbox(ensure(border_left, null_or(is.string))),
          border_right = unbox(ensure(border_right, null_or(is.string))),
          border_top = unbox(ensure(border_top, null_or(is.string))),
          bottom = unbox(ensure(bottom, null_or(is.string))),
          display = unbox(ensure(display, null_or(is.string))),
          flex = unbox(ensure(flex, null_or(is.string))),
          flex_flow = unbox(ensure(flex_flow, null_or(is.string))),
          grid_area = unbox(ensure(grid_area, null_or(is.string))),
          grid_auto_columns = unbox(ensure(grid_auto_columns, null_or(is.string))),
          grid_auto_flow = unbox(ensure(grid_auto_flow, null_or(is.string))),
          grid_auto_rows = unbox(ensure(grid_auto_rows, null_or(is.string))),
          grid_column = unbox(ensure(grid_column, null_or(is.string))),
          grid_gap = unbox(ensure(grid_gap, null_or(is.string))),
          grid_row = unbox(ensure(grid_row, null_or(is.string))),
          grid_template_areas = unbox(ensure(grid_template_areas, null_or(is.string))),
          grid_template_columns = unbox(ensure(grid_template_columns, null_or(is.string))),
          grid_template_rows = unbox(ensure(grid_template_rows, null_or(is.string))),
          height = unbox(ensure(height, null_or(is.string))),
          justify_content = unbox(ensure(justify_content, null_or(is.string))),
          justify_items = unbox(ensure(justify_items, null_or(is.string))),
          left = unbox(ensure(left, null_or(is.string))),
          margin = unbox(ensure(margin, null_or(is.string))),
          max_height = unbox(ensure(max_height, null_or(is.string))),
          max_width = unbox(ensure(max_width, null_or(is.string))),
          min_height = unbox(ensure(min_height, null_or(is.string))),
          min_width = unbox(ensure(min_width, null_or(is.string))),
          object_fit = unbox(ensure(object_fit, null_or(is.string))),
          object_position = unbox(ensure(object_position, null_or(is.string))),
          order = unbox(ensure(order, null_or(is.string))),
          overflow = unbox(ensure(overflow, null_or(is.string))),
          padding = unbox(ensure(padding, null_or(is.string))),
          right = unbox(ensure(right, null_or(is.string))),
          top = unbox(ensure(top, null_or(is.string))),
          visibility = unbox(ensure(visibility, null_or(is.string))),
          width = unbox(ensure(width, null_or(is.string)))
        )

        super$initialize(
          # Widget
          `_model_module` = '@jupyter-widgets/base',
          `_model_module_version` = "2.0.0",
          `_model_name` = "LayoutModel",
          `_view_module` = '@jupyter-widgets/base',
          `_view_count` = NULL,
          `_view_module_version` = "2.0.0",
          `_view_name` = "LayoutView",
          ...,
          error_call = error_call
        )
      }
    )
)
