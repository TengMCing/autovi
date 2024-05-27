
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
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @param keras_mod Keras model. A trained computer vision model.
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
#' * G:
#'    * [AUTO_VI$get_dat()]
#'    * [AUTO_VI$get_fitted_and_resid()]
#' * I:
#'    * [AUTO_VI$..init..()]
#' * L:
#'    * [AUTO_VI$lr_ratio()]
#' * N:
#'    * [AUTO_VI$null_method()]
#'    * [AUTO_VI$null_vss()]
#' * P:
#'    * [AUTO_VI$p_value]
#'    * [AUTO_VI$plot_resid()]
#' * R:
#'    * [AUTO_VI$remove_plot()]
#'    * [AUTO_VI$rotate_resid()]
#' * S:
#'    * [AUTO_VI$save_plot()]
#'    * [AUTO_VI$..str..()]
#'    * [AUTO_VI$summary_plot()]
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
#' AUTO_VI$..init..(fitted_mod, keras_mod = NULL, data = NULL, node_index = 1L)
#' ```
#'
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @param keras_mod Keras model. A trained computer vision model.
#' @param data Data frame. The data used to fit the model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @return Return the object itself.
#'
#' @examples
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
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
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
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
#' AUTO_VI$get_fitted_and_resid(fitted_mod = self$fitted_mod)
#' ```
#'
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @return A tibble.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
#' my_vi$get_fitted_and_resid()
AUTO_VI$get_fitted_and_resid

#' Get data out of a model object
#'
#' @name AUTO_VI$get_dat
#'
#' @description This function gets the data out of a model object
#' by using [stats::model.frame()] if `self$dat` is `NULL`.
#'
#' ## Usage
#' ```
#' AUTO_VI$get_dat(fitted_mod = self$fitted_mod)
#' ```
#'
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @return A tibble.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
#' my_vi$get_dat()
AUTO_VI$get_dat

#' Compute auxiliary variables for the keras model
#'
#' @name AUTO_VI$auxiliary
#'
#' @description This function computes a vector of auxiliary variables
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
#' a vector of values.
#'
#' ## Usage
#' ```
#' AUTO_VI$auxiliary(dat = seflf$get_fitted_and_resid())
#' ```
#'
#' @param data Data frame. A data frame containing variables `.resid` and
#' `.fitted`. See also [AUTO_VI$get_fitted_and_resid()].
#' @return A vector of auxiliary values.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
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
#' @seealso [visage::VI_MODEL]
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
#' my_vi$plot_resid()
AUTO_VI$plot_resid

#' Save a plot
#'
#' @name AUTO_VI$save_plot
#'
#' @description This function saves a plot with [ggplot2::ggsave()].
#'
#' ## Usage
#' ```
#' AUTO_VI$save_plot(
#'   p,
#'   path = tempfile(fileext = ".png"),
#'   width = 7/5,
#'   height = 7/4,
#'   ...
#' )
#' ```
#' @param p `ggplot`. A plot.
#' @param path Character. Path to save. See also [tempfile()].
#' @param width Numeric. Width of the plot.
#' @param height Numeric. Height of the plot.
#' @param ... Arguments passed to [ggplot2::ggsave()].
#' @return A string indicating the path to the file.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
#' my_vi$save_plot(my_vi$plot_resid())
AUTO_VI$save_plot

#' Remove a plot
#'
#' @name AUTO_VI$remove_plot
#'
#' @description This function removes a plot.
#'
#' ## Usage
#' ```
#' AUTO_VI$remove_plot(path, check_ext = TRUE)
#' ```
#' @param path Character. Path to the image file.
#' @param check_ext Boolean. Whether to check the file extension.
#' @return Return the object itself.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
#' temp_path <- my_vi$save_plot(my_vi$plot_resid())
#' my_vi$remove_plot(temp_path)
#' file.exists(temp_path)
AUTO_VI$remove_plot

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
#'   keras_mod = self$keras_mod,
#'   node_index = self$node_index
#' )
#' ```
#'
#' @param p `ggplot`/List. A `ggplot` or a list of `ggplot`.
#' See also [AUTO_VI$plot_resid()].
#' @param auxiliary Numeric. A vector of auxiliary values. This is only used
#' when the keras model has multiple inputs. If it is not provided, the
#' values will be automatically computed based on the residual plot of the
#' fitted model. See also [AUTO_VI$auxiliary()].
#' @param keras_mod Keras model. A trained computer vision model.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @return A numeric vector.
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
#' AUTO_VI$null_method(fitted_mod = self$fitted_mod)
#' ```
#'
#' @param fitted_mod `lm`. A linear model object.
#' @return A tibble with two columns `.fitted` and `.resid`.
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
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
#' AUTO_VI$rotate_resid(fitted_mod = self$fitted_mod)
#' ```
#'
#' @param fitted_mod `lm`. A linear model object.
#' @return A tibble with two columns `.fitted` and `.resid`.
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
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
#'   fitted_mod = self$fitted_mod,
#'   keras_mod = self$keras_mod,
#'   null_method = self$rotate_resid,
#'   node_index = self$node_index,
#'   keep_null_dat = FALSE,
#'   keep_null_plot = FALSE
#' )
#' ```
#'
#' @param draws Integer. Number of simulation draws.
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @param keras_mod Keras model. A trained computer vision model.
#' @param null_method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param keep_null_dat Boolean. Whether to keep the simulated null data.
#' @param keep_null_plot Boolean. Whether to keep the simulated null plots.
#' @return A tibble with 1 to 3 columns depending on the argument
#' `keep_null_dat` and `keep_null_plot`.
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
#'   fitted_mod = self$fitted_mod,
#'   keras_mod = self$keras_mod,
#'   jitter = FALSE,
#'   factor = 1L,
#'   data = self$get_dat(),
#'   node_index = 1L,
#'   keep_boot_dat = FALSE,
#'   keep_boot_plot = FALSE
#' )
#' ```
#'
#' @param draws Integer. Number of simulation draws.
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @param keras_mod Keras model. A trained computer vision model.
#' @param jitter Boolean. Whether to use `jitter()` to generate bootstrapped
#' data instead of sampling from the original data with replacement.
#' @param factor Numeric. See also [jitter()].
#' @param data Data frame. The data used to fit the model.
#' See also [AUTO_VI$get_dat()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param keep_boot_dat Boolean. Whether to keep the bootstrapped data.
#' @param keep_boot_plot Boolean. Whether to keep the bootstrapped plots.
#' @return A tibble with 1 to 3 columns depending on the argument
#' `keep_boot_dat` and `keep_boot_plot`.
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
#'   fitted_mod = self$fitted_mod,
#'   keras_mod = self$keras_mod,
#'   null_method = self$rotate_resid,
#'   jitter = FALSE,
#'   factor = 1L,
#'   data = self$get_dat(),
#'   node_index = self$node_index,
#'   keep_dat = FALSE,
#'   keep_plot = FALSE
#' )
#' ```
#'
#' @param null_draws Integer. Number of simulation draws for
#' [AUTO_VI$null_vss()].
#' @param boot_draws Integer. Number of simulation draws for
#' [AUTO_VI$boot_vss()].
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @param keras_mod Keras model. A trained computer vision model.
#' @param null_method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid()].
#' @param jitter Boolean. Whether to use `jitter()` to generate bootstrapped
#' data instead of sampling from the original data with replacement.
#' @param factor Numeric. See also [jitter()].
#' @param data Data frame. The data used to fit the model.
#' See also [AUTO_VI$get_dat()].
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes.
#' @param keep_dat Boolean. Whether to keep the simulated data.
#' @param keep_plot Boolean. Whether to keep the simulated plots.
#' @return Return the object itself.
AUTO_VI$check

#' Compute the likelihood ratio using the simulated result
#'
#' @name AUTO_VI$lr_ratio
#'
#' @description This function estimates the likelihood
#' of observing the visual signal strength in terms of the bootstrapped
#' distribution and the simulated null distribution, and computes the ratio
#' between these two likelihood.
#'
#' ## Usage
#' ```
#' AUTO_VI$lr_ratio()
#' ```
#'
#' @return A named vector with three elements `boot_likelihood`,
#' `null_likelihood` and `lr_ratio`.
AUTO_VI$lr_ratio

#' Compute the p-value based on the check result
#'
#' @name AUTO_VI$p_value
#'
#' @description This function computes the p-value of observing the
#' visual signal strength of the original residual plot based on the null
#' distribution, and computes the p-value of observing the mean of the
#' bootstrapped distribution based on the null distribution.
#'
#' ## Usage
#' ```
#' AUTO_VI$p_value(type = "null")
#' ```
#'
#' @param type Character. Either "null" or "boot".
#' @return A numeric value representing the desired p-value.
AUTO_VI$p_value


#' Draw a summary plot for the result
#'
#' @name AUTO_VI$summary_plot
#'
#' @description This function draws a summary plot for the result.
#'
#' ## Usage
#' ```
#' AUTO_VI$summary_plot()
#' ```
#'
#' @return A `ggplot`.
AUTO_VI$summary_plot


