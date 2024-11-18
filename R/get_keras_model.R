
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
#' Github repo using [download.file()] and load the model.
#'
#' Note that the "SavedModel" and "keras" formats are not supported in
#' `tensorflow` versions above 2.15, as
#' `reticulate::import("tensorflow")$keras$models$load_model` encounters issues
#' when loading models saved with the Keras 2 API.
#' Instead, using the "npz" format allows for rebuilding the model
#' from scratch and loading the weights from a ".npz" file, offering a more
#' reliable alternative.
#'
#' @param model_name String. The model name. See also [list_keras_model()].
#' @param format String. The model format to download.
#' Either "npz", "SavedModel" or "keras".
#' @return A keras model.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) keras_model$summary()
#'
#' @export
get_keras_model <- function(model_name, format = "npz") {

  if (!format %in% c("npz", "SavedModel", "keras")) stop('The `format` is not one of "npz", "SavedModel" and "keras"!')

  if (format == "npz") {
    return(get_keras_model_npz(model_name))
  }

  check_python_library_available("tensorflow")
  tf <- reticulate::import("tensorflow", convert = TRUE)

  if (as.integer(strsplit(tf$`__version__`, "\\.")[[1]][2]) >= 16) {
    warning(paste0(
      "The Keras model was originally trained on Monash M3 HPC with TensorFlow 2.12. ",
      'The "keras" format of the model is not supported on your current TensorFlow version ',
      tf$`__version__`, '. ',
      'The "npz" format will be used instead.'
    ))

    return(get_keras_model_npz(model_name))
  }

  if (format == "SavedModel") {
    return(get_keras_model_SavedModel(model_name))
  }

  if (format == "keras") {
    return(get_keras_model_dot_keras(model_name))
  }
}

get_keras_model_dot_keras <- function(model_name) {

  check_python_library_available("tensorflow")
  tf <- reticulate::import("tensorflow", convert = TRUE)

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

get_keras_model_SavedModel <- function(model_name) {

  check_python_library_available("tensorflow")
  tf <- reticulate::import("tensorflow", convert = TRUE)

  if (!("character" %in% class(model_name) && length(model_name) == 1)) {
    stop("Argument `model_name` needs to be a character vector of length 1.")
  }

  meta <- list_keras_model()
  target <- meta$volume_path[meta$model_name == model_name]

  if (length(target) == 0) stop(paste0("Can not find keras model named '", model_name, "'!"))

  vol_size <- meta$volume_size[meta$model_name == model_name]

  all_temp_files <- list()
  combined_zip <- tempfile(fileext = ".zip")

  # Get all parts
  for (i in 1:vol_size) {
    current_path <- gsub("001", sprintf("%03d", i), target)
    all_temp_files[[i]] <- tempfile()
    utils::download.file(url = paste0("https://github.com/TengMCing/autovi_data/raw/master/", current_path),
                         destfile = all_temp_files[[i]])
  }

  # Combined them into one zip file
  output_conn <- file(combined_zip, "wb")
  for (part in all_temp_files) {
    part_conn <- file(part, "rb")
    data <- readBin(part_conn, what = raw(), n = file.info(part)$size)
    writeBin(data, output_conn)
    close(part_conn)
  }
  close(output_conn)

  # Unzip the file
  temp_folder <- tempdir()
  utils::unzip(combined_zip, exdir = temp_folder)

  keras <- tf$keras
  mod <- keras$models$load_model(file.path(temp_folder, model_name))

  return(mod)
}


get_keras_model_npz <- function(model_name) {

  check_python_library_available("tensorflow")

  if (!("character" %in% class(model_name) && length(model_name) == 1)) {
    stop("Argument `model_name` needs to be a character vector of length 1.")
  }

  meta <- list_keras_model()

  if (sum(meta$model_name == model_name) == 0) stop(paste0("Can not find keras model named '", model_name, "'!"))

  py_path <- meta$npz_py[meta$model_name == model_name]
  npz_path <- meta$npz_path[meta$model_name == model_name]

  this_rebuild_py <- tempfile(fileext = ".py")
  utils::download.file(url = paste0("https://github.com/TengMCing/autovi_data/raw/master/", py_path),
                       destfile = this_rebuild_py)

  this_npz <- tempfile(fileext = ".npz")
  utils::download.file(url = paste0("https://github.com/TengMCing/autovi_data/raw/master/", npz_path),
                       destfile = this_npz)

  local_dict <- reticulate::py_run_file(file = this_rebuild_py,
                                        local = TRUE,
                                        convert = TRUE)
  return(local_dict$build_model(this_npz))
}
