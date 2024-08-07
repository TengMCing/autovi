% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_auto_visual_inference.R
\name{AUTO_VI$feature_pca_plot}
\alias{AUTO_VI$feature_pca_plot}
\title{Draw a summary Plot for principal component analysis conducted on extracted features}
\arguments{
\item{feature_pca}{Dataframe. A data frame containing the rotated features.}

\item{x}{Symbol. The x variable. See also \link[ggplot2:tidyeval]{ggplot2::tidyeval}.}

\item{y}{Symbol. The y variable. See also \link[ggplot2:tidyeval]{ggplot2::tidyeval}.}

\item{col_by_set}{Boolena. Whether to color points by sets (observed, null,
and boot).}
}
\value{
A \code{ggplot}.
}
\description{
This function draws a summary Plot for principal component
analysis conducted on extracted features
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{AUTO_VI$feature_pca_plot(
  feature_pca = self$feature_pca(),
  x = PC1,
  y = PC2,
  col_by_set = TRUE)
}\if{html}{\out{</div>}}
}
}
\details{
By default, it will visualize PC2 vs PC1. User can choose to
visualize other principal components.
}
\examples{
keras_model <- try(get_keras_model("vss_phn_32"))
if (!inherits(keras_model, "try-error")) {
  myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)

  myvi$lineup_check(extract_feature_from_layer = "global_max_pooling2d")
  myvi$feature_pca_plot()
}

}
