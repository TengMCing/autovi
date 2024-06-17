
# KERAS_WRAPPER -----------------------------------------------------------

KERAS_WRAPPER <- new.env()


#' KERAS_WRAPPER class environment
#'
#' @name KERAS_WRAPPER
#'
#' @description This is the class of keras wrapper,
#' inherited from [bandicoot::BASE]. It is an environment
#' with S3 class `bandicoot_oop`.
#'
#' @param keras_model Keras model. A trained computer vision model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [bandicoot::BASE]
#'
#' ## New methods
#' * G:
#'    * [KERAS_WRAPPER$get_input_height()]
#'    * [KERAS_WRAPPER$get_input_width()]
#' * I:
#'    * [KERAS_WRAPPER$image_to_array()]
#'    * [KERAS_WRAPPER$..init..()]
#' * L:
#'    * [KERAS_WRAPPER$list_layer_name()]
#' * P:
#'    * [KERAS_WRAPPER$predict()]
#' * S:
#'    * [KERAS_WRAPPER$..str..()]
#'
#' @export
KERAS_WRAPPER


#' @describeIn KERAS_WRAPPER Class constructor, same as `KERAS_WRAPPER$instantiate()`.
#' @export
keras_wrapper <- function(keras_model = NULL,
                          node_index = 1L,
                          env = new.env(parent = parent.frame()),
                          init_call = sys.call()) {
  KERAS_WRAPPER$instantiate(keras_model = keras_model,
                            node_index = node_index,
                            env = env,
                            init_call = init_call)
}

#' Initialization method
#'
#' @name KERAS_WRAPPER$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$..init..(keras_mod = NULL, node_index = 1L)
#' ```
#'
#' @param keras_mod Keras model. A trained computer vision model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @return Return the object itself.
#'
#' @examples
#' keras_wrapper()
#'
KERAS_WRAPPER$..init..

#' String representation of the object
#'
#' @name KERAS_WRAPPER$..str..
#'
#' @description This function returns a string representation of the object.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$..str..()
#' ```
#' @return A string.
#'
#' @examples
#'
#' KERAS_WRAPPER$..str..()
#'
#' wrapper <- keras_wrapper()
#' wrapper$..str..()
KERAS_WRAPPER$..str..

#' Predict visual signal strength
#'
#' @name KERAS_WRAPPER$predict
#'
#' @description This function predicts the visual signal strength using the
#' provided keras model, input array and optional auxiliary input array.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$predict(
#'   input_array,
#'   auxiliary = NULL,
#'   keras_model = self$keras_model,
#'   node_index = self$node_index,
#'   extract_featrue_from_layer = NULL
#' )
#' ```
#'
#' @param input_array Array/Numpy array. An input array, usually of the
#' shape (batch_size, height, width, channels).
#' @param auxiliary Array/Data frame. An auxiliary input array of the
#' shape (batch_size, number_of_auxiliary_inputs). This is only needed if the
#' keras model takes multiple inputs.
#' @param keras_model Keras model. A trained computer vision model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param extract_feature_from_layer Character/Integer. A layer name or an
#' integer layer index for extracting features from a layer.
#' @return A tibble. The first column is `vss` which is the prediction, the
#' rest of the columns are features extracted from a layer.
#'
#' @examples
#' if (interactive()) {
#'   keras_model <- get_keras_model("vss_phn_32")
#'   wrapper <- keras_wrapper(keras_model)
#'
#'   # Provide one 32 * 32 RGB image and one vector of length 5
#'   wrapper$predict(input_array = array(255, dim = c(1, 32, 32, 3)),
#'                   auxiliary = matrix(1, ncol = 5))
#' }
#'
KERAS_WRAPPER$predict


#' Get keras model input image height
#'
#' @name KERAS_WRAPPER$get_input_height
#'
#' @description This function get the input image height (the input shape
#' is (batch_size, height, width, channels)) of a keras model.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$get_input_height(keras_model = self$keras_model)
#' ```
#'
#' @param keras_model Keras model. A trained computer vision model.
#' @return An integer.
#'
#' @examples
#' if (interactive()) {
#'   keras_model <- get_keras_model("vss_phn_32")
#'   keras_model$get_input_height()
#' }
#'
KERAS_WRAPPER$get_input_height

#' Get keras model input image width
#'
#' @name KERAS_WRAPPER$get_input_width
#'
#' @description This function get the input image width (the input shape
#' is (batch_size, height, width, channels)) of a keras model.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$get_input_width(keras_model = self$keras_model)
#' ```
#'
#' @param keras_model Keras model. A trained computer vision model.
#' @return An integer.
#'
#' @examples
#' if (interactive()) {
#'   keras_model <- get_keras_model("vss_phn_32")
#'   keras_model$get_input_width()
#' }
#'
KERAS_WRAPPER$get_input_width

#' Load an image as numpy array
#'
#' @name KERAS_WRAPPER$image_to_array
#'
#' @description This function loads an image from file and convert it to a
#' numpy array.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$image_to_array(
#'   path,
#'   height = self$get_input_height(),
#'   width = self$get_input_width()
#' )
#' ```
#'
#' @param path Character. Path to the image.
#' @param height Integer. Target height of the image.
#' @param width Integer. Target width of the image.
#' @return A numpy array.
#'
#' @examples
#' if (interactive()) {
#'   p <- ggplot2::ggplot(cars) + ggplot2::geom_point(ggplot2::aes(dist, speed))
#'   path <- save_plot(p)
#'   KERAS_WRAPPER$image_to_array(path, 32L, 32L)
#' }
#'
KERAS_WRAPPER$image_to_array

#' List all layer names
#'
#' @name KERAS_WRAPPER$list_layer_name
#'
#' @description This function list all layer names of a keras model.
#'
#' ## Usage
#' ```
#' KERAS_WRAPPER$list_layer_name(keras_model = self$keras_model)
#' ```
#'
#' @param keras_model Keras model. A trained computer vision model.
#' @return A vector of strings.
#'
#' @examples
#' if (interactive()) {
#'   keras_model <- get_keras_model("vss_phn_32")
#'   keras_wrapper(keras_model)$list_layer_name()
#' }
#'
KERAS_WRAPPER$image_to_array
