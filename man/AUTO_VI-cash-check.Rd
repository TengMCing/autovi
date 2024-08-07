% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_auto_visual_inference.R
\name{AUTO_VI$check}
\alias{AUTO_VI$check}
\title{Conduct a auto visual inference check with a computer vision model}
\arguments{
\item{null_draws}{Integer. Number of simulation draws for
\code{\link[=AUTO_VI$null_vss]{AUTO_VI$null_vss()}}.}

\item{boot_draws}{Integer. Number of simulation draws for
\code{\link[=AUTO_VI$boot_vss]{AUTO_VI$boot_vss()}}.}

\item{fitted_model}{Model. A model object, e.g. \code{lm}.}

\item{keras_model}{Keras model. A trained computer vision model.}

\item{null_method}{Function. A method to simulate residuals from the null
hypothesis distribution. For \code{lm}, the recommended method is residual
rotation \code{\link[=AUTO_VI$rotate_resid]{AUTO_VI$rotate_resid()}}.}

\item{data}{Data frame. The data used to fit the model.
See also \code{\link[=AUTO_VI$get_data]{AUTO_VI$get_data()}}.}

\item{node_index}{Integer. An index indicating which node of the output layer
contains the visual signal strength. This is particularly useful
when the keras model has more than one output nodes.}

\item{keep_data}{Boolean. Whether to keep the simulated data.}

\item{keep_plot}{Boolean. Whether to keep the simulated plots.}

\item{extract_feature_from_layer}{Character/Integer. A layer name or an
integer layer index for extracting features from a layer.}
}
\value{
Return the object itself.
}
\description{
This function conducts a visual inference
check with a computer vision model. The result will be stored in
\code{self$check_result}.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{AUTO_VI$check(
  null_draws = 100L,
  boot_draws = 100L,
  fitted_model = self$fitted_model,
  keras_model = self$keras_model,
  null_method = self$null_method,
  data = self$get_data(),
  node_index = self$node_index,
  keep_data = FALSE,
  keep_plot = FALSE,
  extract_feature_from_layer = NULL
)
}\if{html}{\out{</div>}}
}
}
\examples{
keras_model <- try(get_keras_model("vss_phn_32"))
if (!inherits(keras_model, "try-error")) {
  myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)

  myvi$check()
  myvi
}
}
