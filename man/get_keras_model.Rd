% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_keras_model.R
\name{get_keras_model}
\alias{get_keras_model}
\title{Download and load the keras model}
\usage{
get_keras_model(model_name, format = "SavedModel")
}
\arguments{
\item{model_name}{String. The model name. See also \code{\link[=list_keras_model]{list_keras_model()}}.}

\item{format}{String. The model format to download.
Either "SavedModel" or "keras".}
}
\value{
A keras model.
}
\description{
This functions download the keras model from the \code{TengMCing/autovi_data}
Github repo using \code{\link[=download.file]{download.file()}} and load the model using
\code{reticulate::import("tensorflow")$keras$models$load_model}. Note that
\code{tensorflow} version greater than 2.15 is not supported.
}
\examples{
keras_model <- try(get_keras_model("vss_phn_32"))
if (!inherits(keras_model, "try-error")) keras_model$summary()

}
