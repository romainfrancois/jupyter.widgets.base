% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Widget.R
\name{Widget}
\alias{Widget}
\title{Widget}
\usage{
Widget(
  `_model_module` = "@jupyter-widgets/base",
  `_model_module_version` = "2.0.0",
  `_model_name` = "",
  `_view_module` = "@jupyter-widgets/base",
  `_view_count` = NULL,
  `_view_module_version` = "2.0.0",
  `_view_name` = "",
  ...,
  error_call = caller_env()
)
}
\arguments{
\item{_model_module}{The namespace of the model.}

\item{_model_module_version}{A semver requirement for namespace version containing the model.}

\item{_model_name}{model name}

\item{_view_module}{view mmodule}

\item{_view_count}{view count}

\item{_view_module_version}{view module version}

\item{_view_name}{view name}

\item{...}{These dots are for future extensions and must be empty.}

\item{error_call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\value{
a \link{jupyter.widget.Widget} object
}
\description{
Widget
}
