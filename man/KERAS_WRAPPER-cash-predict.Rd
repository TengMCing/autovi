% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_keras_wrapper.R
\name{KERAS_WRAPPER$predict}
\alias{KERAS_WRAPPER$predict}
\title{Predict visual signal strength}
\arguments{
\item{input_array}{Array/Numpy array. An input array, usually of the
shape (batch_size, height, width, channels).}

\item{auxiliary}{Array/Data frame. An auxiliary input array of the
shape (batch_size, number_of_auxiliary_inputs). This is only needed if the
keras model takes multiple inputs.}

\item{keras_model}{Keras model. A trained computer vision model.}

\item{node_index}{Integer. An index indicating which node of the output layer
contains the visual signal strength. This is particularly useful
when the keras model has more than one output nodes.}

\item{extract_feature_from_layer}{Character/Integer. A layer name or an
integer layer index for extracting features from a layer.}
}
\value{
A tibble. The first column is \code{vss} which is the prediction, the
rest of the columns are features extracted from a layer.
}
\description{
This function predicts the visual signal strength using the
provided keras model, input array and optional auxiliary input array.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{KERAS_WRAPPER$predict(
  input_array,
  auxiliary = NULL,
  keras_model = self$keras_model,
  node_index = self$node_index,
  extract_featrue_from_layer = NULL
)
}\if{html}{\out{</div>}}
}
}
\examples{
keras_model <- try(get_keras_model("vss_phn_32"))
if (!inherits(keras_model, "try-error")) {
  wrapper <- keras_wrapper(keras_model)

  # Provide one 32 * 32 RGB image and one vector of length 5 as input
  wrapper$predict(input_array = array(255, dim = c(1, 32, 32, 3)),
                  auxiliary = matrix(1, ncol = 5))
}

}
