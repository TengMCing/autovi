class_KERAS_WRAPPER <- function(env = new.env(parent = parent.frame())) {
  self <- NULL

  bandicoot::new_class(bandicoot::BASE, env = env, class_name = "KERAS_WRAPPER")


# init --------------------------------------------------------------------

  init_ <- function(keras_model = NULL, node_index = 1L) {
    self$keras_model <- keras_model
    self$node_index <- node_index

    return(invisible(self))
  }


# predict -----------------------------------------------------------------

  predict_ <- function(input_array,
                       auxiliary = NULL,
                       keras_model = self$keras_model,
                       node_index = self$node_index,
                       extract_feature_from_layer = NULL) {

    autovi::check_python_library_available("tensorflow")
    tf <- reticulate::import("tensorflow", convert = TRUE)
    keras <- tf$keras

    # Check if the keras model have multiple inputs
    mutltiple_inputs_flag <- length(keras_model$inputs) > 1

    # Convert auxiliary input
    if (is.data.frame(auxiliary)) {
      auxiliary <- reticulate::np_array(as.matrix(auxiliary))
    }

    if (is.matrix(auxiliary)) {
      auxiliary <- reticulate::np_array(auxiliary)
    }

    # Predict the batch.
    if (mutltiple_inputs_flag) {
      output <- keras_model$`__call__`(list(input_array, auxiliary))$numpy()
    } else {
      output <- keras_model$`__call__`(input_array)$numpy()
    }

    cli::cli_alert_success("Predict visual signal strength for {dim(input_array)[1]} image{?s}.")

    if ("python.builtin.object" %in% class(output)) {
      output <- reticulate::py_to_r(output)
    }

    # Extract the value of a particular output node.
    if (!(is.vector(output) && is.atomic(output))) output <- output[, node_index]

    # Use the model as a feature extractor
    if (!is.null(extract_feature_from_layer)) {

      if(!(is.numeric(extract_feature_from_layer) || is.character(extract_feature_from_layer))) {
        stop("Argument `extract_feature_from_layer` needs to be an integer or a string.")
      }

      if (is.numeric(extract_feature_from_layer))
        target_layer <- keras_model$get_layer(index = as.integer(extract_feature_from_layer) - 1L)

      if (is.character(extract_feature_from_layer))
        target_layer <- keras_model$get_layer(extract_feature_from_layer)

      # Build a feature extraction model
      feature_mod <- keras$Model(inputs = keras_model$input, outputs = target_layer$output)

      # Extract feature
      if (mutltiple_inputs_flag) {
        feature <- feature_mod$`__call__`(list(input_array, auxiliary))$numpy()
      } else {
        feature <- feature_mod$`__call__`(input_array)$numpy()
      }

      if ("python.builtin.object" %in% class(feature)) {
        feature <- reticulate::py_to_r(feature)
      }

      # Convert the array into a matrix
      dim(feature) <- c(dim(feature)[1], prod(dim(feature))/dim(feature)[1])
      colnames(feature) <- paste0("f_", 1:dim(feature)[2])

      return(tibble::as_tibble(cbind(tibble::tibble(vss = output),
                                     tibble::as_tibble(feature))))
    } else {
      return(tibble::tibble(vss = output))
    }
  }


# get_input_height --------------------------------------------------------

  get_input_height_ <- function(keras_model = self$keras_model) {
    if (length(keras_model$inputs) > 1) {
      return(keras_model$input_shape[[1]][[2]])
    } else {
      return(keras_model$input_shape[[2]])
    }
  }


# get_input_width ---------------------------------------------------------

  get_input_width_ <- function(keras_model = self$keras_model) {
    if (length(keras_model$inputs) > 1) {
      return(keras_model$input_shape[[1]][[3]])
    } else {
      return(keras_model$input_shape[[3]])
    }
  }

# image_to_array ----------------------------------------------------------

  image_to_array_ <- function(path,
                              height = self$get_input_height(),
                              width = self$get_input_width()) {

    autovi::check_python_library_available("numpy")
    autovi::check_python_library_available("PIL")
    autovi::check_python_library_available("tensorflow")
    np <- reticulate::import("numpy", convert = FALSE)
    PIL <- reticulate::import("PIL", convert = FALSE)
    tf <- reticulate::import("tensorflow", convert = TRUE)
    keras <- tf$keras

    # Init the input batch.
    input_batch <- vector(mode = "list", length = length(path))

    i <- 0
    for (this_path in path) {
      i <- i + 1

      # Load the image and resize it to the correct size.
      input_image <- PIL$Image$open(this_path)$resize(c(width, height))

      # Convert the image to a Numpy array.
      input_array <- keras$utils$img_to_array(input_image)

      # Store the array into the batch
      input_batch[[i]] <- input_array
    }

    # Convert a list of Numpy arrays to a batch.
    input_batch <- np$stack(input_batch)

    return(input_batch)
  }


# list_layer_name ---------------------------------------------------------

  list_layer_name_ <- function(keras_model = self$keras_model) {
    all_name <- c()
    for (layer in myvi$keras_model$layers) {
      all_name <- c(all_name, layer$name)
    }
    return(all_name)
  }


# str ---------------------------------------------------------------------

  str_ <- function() {
    # Check if the object is instantiated.
    if (!self$..instantiated..) {
      return(paste0("<", self$..type.., " class>"))
    }

    # Borrow the string format from BASE.
    result <- bandicoot::use_method(self, bandicoot::BASE$..str..)()

    # Report the status.
    result <- paste0(result, "\n Status:")

    # Report the keras model.
    if (is.null(self$keras_model)) {
      keras_model_status <- "UNKNOWN"
    } else {
      input_shape <- paste(unlist(self$keras_model$input_shape[[1]]), collapse = ", ")
      output_shape <- paste(unlist(self$keras_model$output_shape), collapse = ", ")

      if (length(self$keras_model$inputs) == 1) {
        keras_model_status <- paste0("(None, ", input_shape, ") -> ", "(None, ", output_shape, ")")
      } else {
        second_input_shape <- unlist(self$keras_model$input_shape[[2]])
        keras_model_status <- paste0("(None, ", input_shape, ") + (None, ", second_input_shape, ") -> ", "(None, ", output_shape, ")")
      }

    }
    result <- paste0(result, "\n  - Keras model: ", keras_model_status)

    # Report the output node of the keras model.
    if (!is.null(self$node_index)) {
      result <- paste0(result, "\n     - Output node index: ", self$node_index)
    }

    return(result)
  }

  bandicoot::register_method(env,
                             ..init.. = init_,
                             predict = predict_,
                             get_input_height = get_input_height_,
                             get_input_width = get_input_width_,
                             image_to_array = image_to_array_,
                             list_layer_name = list_layer_name_,
                             ..str.. = str_)

}
