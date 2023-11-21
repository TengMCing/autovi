
# list_keras_models -------------------------------------------------------

#' List all available pre-trained computer vision models
#'
#' This function gets a table of available pre-trained computer vision models
#' for predicting visual signal strength.
#'
#' @return A tibble of available model names and paths.
#' @examples
#'
#' list_keras_models()
#'
#' @export
list_keras_models <- function() {
  meta <- utils::read.csv("https://raw.githubusercontent.com/TengMCing/autovi_data/master/available_models.csv")
  tibble::as_tibble(meta)
}

# get_keras_model ---------------------------------------------------------

#' Download and load the keras model
#'
#' This functions download the keras model from the `TengMCing/autovi_data`
#' Github repo using [download.file()] and load the model using
#' `keras::keras$models$load_model`.
#'
#' @param model_name String. The model name. See also [list_keras_models()].
#' @return A keras model.
#'
#' @export
get_keras_model <- function(model_name) {
  meta <- list_keras_models()
  target <- meta$path[meta$model_name == model_name]

  if (length(target) == 0) stop(paste0("Can not find keras model named '", model_name, "'!"))

  temp <- tempfile(fileext = ".zip")
  utils::download.file(url = paste0("https://github.com/TengMCing/autovi_data/raw/master/", target),
                       destfile = temp)
  temp_folder <- tempdir()
  utils::unzip(temp, exdir = temp_folder)

  mod <- keras::keras$models$load_model(file.path(temp_folder, paste0(model_name, ".keras")))

  return(mod)
}
