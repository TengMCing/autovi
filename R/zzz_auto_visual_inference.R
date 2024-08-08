
# AUTO_VI -----------------------------------------------------------------

AUTO_VI <- new.env()

#' AUTO_VI class environment
#'
#' @name AUTO_VI
#'
#' @description This is the class of auto visual inference,
#' inherited from [bandicoot::BASE]. It is an environment
#' with S3 class `bandicoot_oop`.
#'
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @param keras_model Keras model. A trained computer vision model.
#' @param data Data frame. The data used to fit the model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [bandicoot::BASE]
#'
#' ## New attributes
#' * C:
#'    * [AUTO_VI$check_result]
#'
#' ## New methods
#' * A:
#'    * [AUTO_VI$auxiliary()]
#' * B:
#'    * [AUTO_VI$boot_vss()]
#' * C:
#'    * [AUTO_VI$check()]
#' * F:
#'    * [AUTO_VI$feature_pca()]
#'    * [AUTO_VI$feature_pca_plot()]
#' * G:
#'    * [AUTO_VI$get_data()]
#'    * [AUTO_VI$get_fitted_and_resid()]
#' * I:
#'    * [AUTO_VI$..init..()]
#' * L:
#'    * [AUTO_VI$lineup_check()]
#'    * [AUTO_VI$likelihood_ratio()]
#' * N:
#'    * [AUTO_VI$null_method()]
#'    * [AUTO_VI$null_vss()]
#' * P:
#'    * [AUTO_VI$p_value()]
#'    * [AUTO_VI$plot_resid()]
#' * R:
#'    * [AUTO_VI$rotate_resid()]
#' * S:
#'    * [AUTO_VI$save_plot()]
#'    * [AUTO_VI$select_feature()]
#'    * [AUTO_VI$..str..()]
#'    * [AUTO_VI$summary_density_plot()]
#'    * [AUTO_VI$summary_plot()]
#'    * [AUTO_VI$summary_rank_plot()]
#' * V:
#'    * [AUTO_VI$vss()]
#'
#' @export
AUTO_VI

#' @describeIn AUTO_VI Class constructor, same as `AUTO_VI$instantiate()`.
#' @export
auto_vi <- function(fitted_model,
                    keras_model = NULL,
                    data = NULL,
                    node_index = 1L,
                    env = new.env(parent = parent.frame()),
                    init_call = sys.call()) {
  AUTO_VI$instantiate(fitted_model = fitted_model,
                      keras_model = keras_model,
                      data = data,
                      node_index = node_index,
                      env = env,
                      init_call = init_call)
}

#' List of diagnostic results
#'
#' @name AUTO_VI$check_result
#'
#' @description A list, will be initialized after the method [AUTO_VI$check()]
#' is run.
AUTO_VI$check_result

#' Initialization method
#'
#' @name AUTO_VI$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' AUTO_VI$..init..(fitted_model, keras_model = NULL, data = NULL, node_index = 1L)
#' ```
#'
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @param keras_model Keras model. A trained computer vision model.
#' @param data Data frame. The data used to fit the model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @return Return the object itself.
#'
#' @examples
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' my_vi
AUTO_VI$..init..

#' String representation of the object
#'
#' @name AUTO_VI$..str..
#'
#' @description This function returns a string representation of the object.
#'
#' ## Usage
#' ```
#' AUTO_VI$..str..()
#' ```
#' @return A string.
#'
#' @examples
#'
#' AUTO_VI$..str..()
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' my_vi$..str..()
AUTO_VI$..str..

#' Get fitted values and residuals out of a model object
#'
#' @name AUTO_VI$get_fitted_and_resid
#'
#' @description This function gets fitted values and residuals
#' out of a model object by using [stats::fitted()] and [stats::resid()].
#'
#' ## Usage
#' ```
#' AUTO_VI$get_fitted_and_resid(fitted_model = self$fitted_model)
#' ```
#'
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @return A tibble.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' my_vi$get_fitted_and_resid()
AUTO_VI$get_fitted_and_resid

#' Get data out of a model object
#'
#' @name AUTO_VI$get_data
#'
#' @description This function gets the data out of a model object
#' by using [stats::model.frame()] if `self$data` is `NULL`.
#'
#' ## Usage
#' ```
#' AUTO_VI$get_data(fitted_model = self$fitted_model)
#' ```
#'
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @return A tibble.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' my_vi$get_data()
AUTO_VI$get_data

#' Compute auxiliary variables for the keras model
#'
#' @name AUTO_VI$auxiliary
#'
#' @description This function computes auxiliary variables
#' including monotonic measure (`measure_monotonic`),
#' sparse measure (`measure_sparse`), splines measure (`measure_splines`),
#' striped measure (`measure_striped`), and the number of observation (`n`).
#' Scagnostics are computed using
#' [cassowaryr::sc_monotonic()], [cassowaryr::sc_sparse2()], [cassowaryr::sc_splines()],
#' and [cassowaryr::sc_striped()].
#'
#' If you wish to calculate additional auxiliary variables for your keras
#' model, please override this method. Ensure that it accepts a data frame
#' with columns named `.fitted` and `.resid` as input and returns
#' a single row tibble.
#'
#' ## Usage
#' ```
#' AUTO_VI$auxiliary(data = seflf$get_fitted_and_resid())
#' ```
#'
#' @param data Data frame. A data frame containing variables `.resid` and
#' `.fitted`. See also [AUTO_VI$get_fitted_and_resid()].
#' @return A tibble.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' my_vi$auxiliary()
AUTO_VI$auxiliary

#' Draw a standard residual plot
#'
#' @name AUTO_VI$plot_resid
#'
#' @description This function draws a standard residual plot.
#'
#' ## Usage
#' ```
#' AUTO_VI$plot_resid(
#'   data = self$get_fitted_and_resid(),
#'   theme = ggplot2::theme_light(base_size = 11/5),
#'   alpha = 1,
#'   size = 0.5,
#'   stroke = 0.5,
#'   remove_axis = TRUE,
#'   remove_legend = TRUE,
#'   remove_grid_line = TRUE,
#'   add_zero_line = TRUE
#' )
#' ```
#'
#' @param data Data frame. A data frame containing variables `.resid` and
#' `.fitted`. See also [AUTO_VI$get_fitted_and_resid()].
#' @param theme `ggtheme`. A `ggplot` theme object.
#' See also [ggplot2::theme_light()].
#' @param alpha Numeric. Alpha of dot. Value between 0 and 1.
#' @param size Numeric. Size of dot. Value between 0 and 1.
#' @param stroke Numeric. Stroke of dot. Value between 0 and 1.
#' @param remove_axis Boolean. Whether or not to remove the axis.
#' @param remove_legend Boolean. Whether or not to remove the legend.
#' @param remove_grid_line Boolean. Whether or not to remove the grid lines.
#' @param add_zero_line Boolean. Whether or not to add a zero horizontal line.
#' @return A `ggplot`.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' my_vi$plot_resid()
AUTO_VI$plot_resid

#' Save a plot
#'
#' @name AUTO_VI$save_plot
#'
#' @description This is the default method of saving a plot. It will use
#' [save_plot()] to save the ggplot to a 420 (width) * 525 (height) PNG
#' file. If the trained images are generated differently, one can override
#' this method using [bandicoot::register_method()].
#'
#' ## Usage
#' ```
#' AUTO_VI$save_plot(p)
#' ```
#'
#' @param p ggplot. A plot.
#' @return The image path.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' p <- my_vi$plot_resid()
#' my_vi$save_plot(p)
AUTO_VI$save_plot


#' Predict the visual signal strength
#'
#' @name AUTO_VI$vss
#'
#' @description This function predicts the visual signal strength.
#'
#' ## Usage
#' ```
#' AUTO_VI$vss(
#'   p = self$plot_resid(),
#'   auxiliary = NULL,
#'   keras_model = self$keras_model,
#'   node_index = self$node_index,
#'   extract_feature_from_layer = NULL
#' )
#' ```
#'
#' @param p `ggplot`/List/Data.frame/Array/Numpy array/String. The input can be
#' 1. a `ggplot`,
#' 2. a list of `ggplot`,
#' 3. a data.frame containing
#' `.resid` (residuals) and `.fitted` (fitted values) that can be passed to
#' [AUTO_VI$plot_resid()],
#' 4. a 3D array representing an image,
#' 5. a 4D array representing one or more images,
#' 6. a path to an image,
#' 7. a vector or a list of paths to images,
#' 8. a numpy array.
#' @param auxiliary Dataframe. A dataframe of auxiliary values. This is only used
#' when the keras model has multiple inputs. If it is not provided, the
#' values will be automatically computed based on the residual plot of the
#' fitted model. See also [AUTO_VI$auxiliary()].
#' @param keras_model Keras model. A trained computer vision model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param extract_feature_from_layer Character/Integer. A layer name or an
#' integer layer index for extracting features from a layer.
#' @return A tibble. The first column is `vss` which is the prediction, the
#' rest of the columns are features extracted from a layer.
#'
#'
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$vss()
#' }
#'
AUTO_VI$vss

#' Get null residuals from a fitted model
#'
#' @name AUTO_VI$null_method
#'
#' @description This default method gets rotated residuals from a fitted linear
#' model using [AUTO_VI$rotate_resid]. User needs to override this method if
#' the fitted model is not a linear regression model.
#'
#' ## Usage
#' ```
#' AUTO_VI$null_method(fitted_model = self$fitted_model)
#' ```
#'
#' @param fitted_model `lm`. A linear model object.
#' @return A tibble with two columns `.fitted` and `.resid`.
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' null_resid <- my_vi$null_method()
#' my_vi$plot_resid(null_resid)
AUTO_VI$null_method

#' Get rotated residuals from a fitted linear model
#'
#' @name AUTO_VI$rotate_resid
#'
#' @description This function gets rotated residuals from a fitted linear
#' model. The rotated residuals are generated by first regressing random
#' noises on the original regressors, then multiply the obtained residuals by
#' original RSS divided by the current RSS. The results are the rotated
#' residuals.
#'
#' ## Usage
#' ```
#' AUTO_VI$rotate_resid(fitted_model = self$fitted_mod)
#' ```
#'
#' @param fitted_model `lm`. A linear model object.
#' @return A tibble with two columns `.fitted` and `.resid`.
#' @examples
#'
#' my_vi <- auto_vi(fitted_model = lm(speed ~ dist, data = cars))
#' rotated_resid <- my_vi$rotate_resid()
#' my_vi$plot_resid(rotated_resid)
AUTO_VI$rotate_resid


#' Simulate null plots and predict the visual signal strength
#'
#' @name AUTO_VI$null_vss
#'
#' @description This function simulates null plots from the null hypothesis
#' distribution, and predicts the visual signal strength.
#'
#' ## Usage
#' ```
#' AUTO_VI$null_vss(
#'   draws = 100L,
#'   fitted_model = self$fitted_model,
#'   keras_model = self$keras_model,
#'   null_method = self$null_method,
#'   node_index = self$node_index,
#'   keep_null_data = FALSE,
#'   keep_null_plot = FALSE,
#'   extract_feature_from_layer = NULL
#' )
#' ```
#'
#' @param draws Integer. Number of simulation draws.
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @param keras_model Keras model. A trained computer vision model.
#' @param null_method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param keep_null_data Boolean. Whether to keep the simulated null data.
#' @param keep_null_plot Boolean. Whether to keep the simulated null plots.
#' @param extract_feature_from_layer Character/Integer. A layer name or an
#' integer layer index for extracting features from a layer.
#' @return A tibble.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$null_vss()
#' }
AUTO_VI$null_vss

#' Predict visual signal strength for bootstrapped residual plots
#'
#' @name AUTO_VI$boot_vss
#'
#' @description This function bootstrap the data and refits the model, then
#' predicts the visual signal strength of the bootstrapped residual plots.
#'
#' ## Usage
#' ```
#' AUTO_VI$boot_vss(
#'   draws = 100L,
#'   fitted_model = self$fitted_model,
#'   keras_model = self$keras_model,
#'   data = self$get_data(),
#'   node_index = 1L,
#'   keep_boot_data = FALSE,
#'   keep_boot_plot = FALSE,
#'   extract_feature_from_layer = NULL
#' )
#' ```
#'
#' @param draws Integer. Number of simulation draws.
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @param keras_model Keras model. A trained computer vision model.
#' @param data Data frame. The data used to fit the model.
#' See also [AUTO_VI$get_data()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param keep_boot_data Boolean. Whether to keep the bootstrapped data.
#' @param keep_boot_plot Boolean. Whether to keep the bootstrapped plots.
#' @param extract_feature_from_layer Character/Integer. A layer name or an
#' integer layer index for extracting features from a layer.
#' @return A tibble.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$boot_vss()
#' }
AUTO_VI$boot_vss

#' Conduct a auto visual inference check with a computer vision model
#'
#' @name AUTO_VI$check
#'
#' @description This function conducts a visual inference
#' check with a computer vision model. The result will be stored in
#' `self$check_result`.
#'
#' ## Usage
#' ```
#' AUTO_VI$check(
#'   null_draws = 100L,
#'   boot_draws = 100L,
#'   fitted_model = self$fitted_model,
#'   keras_model = self$keras_model,
#'   null_method = self$null_method,
#'   data = self$get_data(),
#'   node_index = self$node_index,
#'   keep_data = FALSE,
#'   keep_plot = FALSE,
#'   extract_feature_from_layer = NULL
#' )
#' ```
#'
#' @param null_draws Integer. Number of simulation draws for
#' [AUTO_VI$null_vss()].
#' @param boot_draws Integer. Number of simulation draws for
#' [AUTO_VI$boot_vss()].
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @param keras_model Keras model. A trained computer vision model.
#' @param null_method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid()].
#' @param data Data frame. The data used to fit the model.
#' See also [AUTO_VI$get_data()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param keep_data Boolean. Whether to keep the simulated data.
#' @param keep_plot Boolean. Whether to keep the simulated plots.
#' @param extract_feature_from_layer Character/Integer. A layer name or an
#' integer layer index for extracting features from a layer.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$check()
#'   myvi
#' }
#' @return Return the object itself.
AUTO_VI$check


#' Conduct a auto visual inference lineup check with a computer vision model
#'
#' @name AUTO_VI$lineup_check
#'
#' @description This function conducts a visual inference lineup
#' check with a computer vision model. The result will be stored in
#' `self$check_result`.
#'
#' ## Usage
#' ```
#' AUTO_VI$lineup_check(
#'   lineup_size = 20L,
#'   fitted_model = self$fitted_model,
#'   keras_model = self$keras_model,
#'   null_method = self$null_method,
#'   data = self$get_data(),
#'   node_index = self$node_index,
#'   extract_feature_from_layer = NULL
#' )
#' ```
#'
#' @param lineup_size Integer. Number of plots in a lineup.
#' @param fitted_model Model. A model object, e.g. `lm`.
#' @param keras_model Keras model. A trained computer vision model.
#' @param null_method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid()].
#' @param data Data frame. The data used to fit the model.
#' See also [AUTO_VI$get_data()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param extract_feature_from_layer Character/Integer. A layer name or an
#' integer layer index for extracting features from a layer.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$lineup_check()
#'   myvi
#' }
#'
#' @return Return the object itself.
AUTO_VI$lineup_check

#' Compute the likelihood ratio using the simulated result
#'
#' @name AUTO_VI$likelihood_ratio
#'
#' @description This function estimates the likelihood
#' of observing the visual signal strength in terms of the bootstrapped
#' distribution and the simulated null distribution, and computes the ratio
#' between these two likelihood.
#'
#' ## Usage
#' ```
#' AUTO_VI$likelihood_ratio(
#'   vss = self$check_result$observed$vss,
#'   dist_1 = self$check_result$boot$vss,
#'   dist_2 = self$check_result$null$vss
#' )
#' ```
#'
#' @param vss Numeric. The observed visual signal strength.
#' @param dist_1 Numeric. A vector of visual signal strength for plots following
#' the first distribution (bootstrap distribution by default).
#' @param dist_2 Numeric. A vector of visual signal strength for plots following
#' the second distribution (null distribution by default).
#' @return A named vector with three elements `likelihood_1`,
#' `likelihood_2` and `likelihood_ratio`.
#' @examples
#'
#' dist_1 <- rnorm(100, 0, 1)
#' dist_2 <- rnorm(100, 1, 1)
#' AUTO_VI$likelihood_ratio(0, dist_1, dist_2)
AUTO_VI$likelihood_ratio

#' Compute the p-value based on the check result
#'
#' @name AUTO_VI$p_value
#'
#' @description This function computes the p-value of observing the
#' visual signal strength of the original residual plot based on the null
#' distribution.
#'
#'
#' ## Usage
#' ```
#' AUTO_VI$p_value(
#'   vss = self$check_result$observed$vss,
#'   null_dist = self$check_result$null$vss
#' )
#' ```
#'
#' @param vss Numrice. A single numeric value indicating the visual signal
#' strength for the true residual plot.
#' @param null_dist Numeric. A vector of numeric values indicating the visual
#' signal strength for null residual plots.
#' @return A numeric value representing the desired p-value.
#' @examples
#' vss <- 1
#' null_dist <- rnorm(100, 0, 1)
#' AUTO_VI$p_value(vss, null_dist)
AUTO_VI$p_value

#' Draw a summary plot for the result
#'
#' @name AUTO_VI$summary_plot
#'
#' @description This function draws a summary plot for the result.
#'
#' ## Usage
#' ```
#' AUTO_VI$summary_plot(type = "auto", ...)
#' ```
#'
#' @param type Character. Either "auto", "density" or "rank". Option "auto"
#' will use the Boolean flag `self$check_result$lineup_check`
#' to determine the correct option. See also [AUTO_VI$summary_density_plot()]
#' and [AUTO_VI$summary_rank_plot()].
#' @param ... Arguments passed to [AUTO_VI$summary_density_plot()] or
#' [AUTO_VI$summary_rank_plot()].
#' @return A `ggplot`.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$lineup_check()
#'   myvi$summary_plot()
#' }
#'
AUTO_VI$summary_plot

#' Draw a summary density plot for the result
#'
#' @name AUTO_VI$summary_density_plot
#'
#' @description This function draws a summary density plot for the result.
#'
#' ## Usage
#' ```
#' AUTO_VI$summary_plot(
#'   vss = self$check_result$observed$vss,
#'   null_dist = self$check_result$null$vss,
#'   boot_dist = self$check_result$boot$vss,
#'   p_value = self$check_result$p_value,
#'   likelihood_ratio = self$check_result$likelihood_ratio,
#'   density_alpha = 0.6
#' )
#' ```
#'
#' @param vss Numeric. Observed visual signal strength.
#' @param null_dist Numeric. Null visual signal strength.
#' @param boot_dist Numeric. Bootstrapped visual signal strength.
#' @param p_value Numeric. P-value of the visual test. See also
#' [AUTO_VI$p_value()].
#' @param likelihood_ratio Numeric. Likelihood ratio of the visual test. See
#' also [AUTO_VI$likelihood_ratio()].
#' @param density_alpha Numeric. Alpha value for the density.
#' @return A `ggplot`.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$check()
#'   myvi$summary_density_plot()
#' }
#'
AUTO_VI$summary_density_plot

#' Draw a summary rank plot for the result
#'
#' @name AUTO_VI$summary_rank_plot
#'
#' @description This function draws a summary rank plot for the result.
#'
#' ## Usage
#' ```
#' AUTO_VI$summary_plot(
#'   vss = self$check_result$observed$vss,
#'   null_dist = self$check_result$null$vss,
#'   p_value = self$check_result$p_value
#' )
#' ```
#'
#' @param vss Numeric. Observed visual signal strength.
#' @param null_dist Numeric. Null visual signal strength.
#' @param p_value Numeric. P-value of the visual test. See also
#' [AUTO_VI$p_value()].
#' @return A `ggplot`.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$lineup_check()
#'   myvi$summary_rank_plot()
#' }
#'
AUTO_VI$summary_rank_plot

#' Conduct principal component analysis for features extracted from keras model
#'
#' @name AUTO_VI$feature_pca
#'
#' @description This function conducts principal component analysis for
#' features extracted from keras model.
#'
#' ## Usage
#' ```
#' AUTO_VI$feature_pca(
#'   feature = self$select_feature(self$check_result$observed),
#'   null_feature = self$select_feature(self$check_result$null),
#'   boot_feature = self$select_feature(self$check_result$boot),
#'   center = TRUE,
#'   scale = TRUE
#' )
#' ```
#'
#' @details Features need to be extracted while running the method
#' [AUTO_VI$check()] and [AUTO_VI$lineup_check()] by providing the argument
#' `extract_feature_from_layer`. Features with zero variance will be ignored
#' from the analysis. See also [stats::prcomp()].
#'
#'
#' @param feature Dataframe. A data frame where columns represent
#' features and rows represent observations. It should have only one row.
#' @param null_feature Dataframe. A data frame where columns represent
#' features and rows represent observations. These features are extracted
#' during the evaluation of null plots.
#' @param boot_feature Dataframe. A data frame where columns represent
#' features and rows represent observations. These features are extracted
#' during the evaluation of bootstrapped plots.
#' @param center Boolean. Whether to subtract the mean from the feature.
#' @param scale Boolean. Whether to divide the feature by its standard
#' deviation.
#' @return A tibble of the raw features and the rotated features with
#' attributes `sdev` and `rotation` representing the
#' standard deviation of the principal
#' components and the rotation matrix respectively.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$lineup_check(extract_feature_from_layer = "global_max_pooling2d")
#'   myvi$feature_pca()
#' }
#'
AUTO_VI$feature_pca

#' Select features from the check result
#'
#' @name AUTO_VI$select_feature
#'
#' @description This function select features from the check result.
#'
#' ## Usage
#' ```
#' AUTO_VI$feature_pca(data = self$check_result$observed, pattern = "f_")
#' ```
#'
#' @details By default, features are assumed to follow the naming convention
#' "f_(index)", where index is from one to the number of features.
#'
#' @param data Dataframe. A data frame where some columns represent
#' features and rows represent observations.
#' @param pattern Character. A regrex pattern to search for features.
#' See also [grep()].
#' @return A tibble where columns represent
#' features and rows represent observations.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$lineup_check(extract_feature_from_layer = "global_max_pooling2d")
#'   myvi$select_feature()
#' }
#'
AUTO_VI$select_feature

#' Draw a summary Plot for principal component analysis conducted on extracted features
#'
#' @name AUTO_VI$feature_pca_plot
#'
#' @description This function draws a summary Plot for principal component
#' analysis conducted on extracted features
#'
#' ## Usage
#' ```
#' AUTO_VI$feature_pca_plot(
#'   feature_pca = self$feature_pca(),
#'   x = PC1,
#'   y = PC2,
#'   col_by_set = TRUE)
#' ```
#'
#' @details By default, it will visualize PC2 vs PC1. User can choose to
#' visualize other principal components.
#'
#' @param feature_pca Dataframe. A data frame containing the rotated features.
#' @param x Symbol. The x variable. See also [ggplot2::tidyeval].
#' @param y Symbol. The y variable. See also [ggplot2::tidyeval].
#' @param col_by_set Boolena. Whether to color points by sets (observed, null,
#' and boot).
#' @return A `ggplot`.
#' @examples
#' keras_model <- try(get_keras_model("vss_phn_32"))
#' if (!inherits(keras_model, "try-error")) {
#'   myvi <- auto_vi(lm(dist ~ speed, data = cars), keras_model)
#'
#'   myvi$lineup_check(extract_feature_from_layer = "global_max_pooling2d")
#'   myvi$feature_pca_plot()
#' }
#'
AUTO_VI$feature_pca_plot
