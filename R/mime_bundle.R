
#' @importFrom hera mime_types
#' @export
mime_types.jupyter.widget.DOMWidget <- function(x) {
  c("application/vnd.jupyter.widget-view+json")
}

#' @importFrom hera mime_bundle
#' @export
mime_bundle.jupyter.widget.DOMWidget <- function(x, mimetypes = mime_types(x), ...) {
  x$mime_bundle()
}
