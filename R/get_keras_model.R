
# list_keras_model --------------------------------------------------------

#' List all available pre-trained computer vision models
#'
#' This function gets a table of available pre-trained computer vision models
#' for predicting visual signal strength.
#'
#' @return A tibble of available model names and paths.
#' @examples
#'
#' list_keras_model()
#'
#' @export
list_keras_model <- function() {
  meta <- utils::read.csv("https://raw.githubusercontent.com/TengMCing/autovi_data/master/available_models.csv")
  tibble::as_tibble(meta)
}

# get_keras_model ---------------------------------------------------------

#' Download and load the keras model
#'
#' This functions download the keras model from the `TengMCing/autovi_data`
#' Github repo using [download.file()] and load the model using
#' `reticulate::import("tensorflow")$keras$models$load_model`. Note that
#' `tensorflow` version greater than 2.15 is not supported.
#'
#' @param model_name String. The model name. See also [list_keras_model()].
#' @return A keras model.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) keras_model$summary()
#'
#' @export
get_keras_model <- function(model_name) {

  check_python_library_available("tensorflow")
  tf <- reticulate::import("tensorflow", convert = TRUE)

  if (as.integer(strsplit(tf$`__version__`, "\\.")[[1]][2]) >= 16) {
    warning(paste0(
      "The Keras model was originally trained on Monash M3 HPC with TensorFlow 2.12 and is tested to work up to TensorFlow 2.15. ",
      "Your current TensorFlow version is ", tf$`__version__`, ", which is not supported. ",
      "Please downgrade your TensorFlow version or install TensorFlow <2.15 in a virtual environment. ",
      "You can use `reticulate::use_python` to select the appropriate virtual environment."
    ))
  }

  if (!("character" %in% class(model_name) && length(model_name) == 1)) {
    stop("Argument `model_name` needs to be a character vector of length 1.")
  }

  meta <- list_keras_model()
  target <- meta$path[meta$model_name == model_name]

  if (length(target) == 0) stop(paste0("Can not find keras model named '", model_name, "'!"))

  temp <- tempfile(fileext = ".zip")
  utils::download.file(url = paste0("https://github.com/TengMCing/autovi_data/raw/master/", target),
                       destfile = temp)
  temp_folder <- tempdir()
  utils::unzip(temp, exdir = temp_folder)

  keras <- tf$keras
  mod <- keras$models$load_model(file.path(temp_folder, paste0(model_name, ".keras")))

  return(mod)
}
