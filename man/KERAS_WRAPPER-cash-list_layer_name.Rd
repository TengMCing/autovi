% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_keras_wrapper.R
\name{KERAS_WRAPPER$list_layer_name}
\alias{KERAS_WRAPPER$list_layer_name}
\title{List all layer names}
\arguments{
\item{keras_model}{Keras model. A trained computer vision model.}
}
\value{
A vector of strings.
}
\description{
This function list all layer names of a keras model.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{KERAS_WRAPPER$list_layer_name(keras_model = self$keras_model)
}\if{html}{\out{</div>}}
}
}
\examples{
keras_model <- try(get_keras_model("vss_phn_32"))
if (!inherits(keras_model, "try-error")) {
  keras_wrapper(keras_model)$list_layer_name()
}

}
