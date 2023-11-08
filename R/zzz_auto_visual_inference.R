
# AUTO_VI -----------------------------------------------------------------


#' AUTO_VI class environment
#'
#' @name AUTO_VI
#'
#' @description This is the class of auto visual inference,
#' inherited from [bandicoot::BASE].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [bandicoot::BASE]
#' \cr
#' \cr
#' New attributes: [AUTO_VI$check_result]
#' \cr
#' \cr
#' New methods: [AUTO_VI$..init..], [AUTO_VI$..str..],
#' [AUTO_VI$get_fitted_and_resid], [AUTO_VI$get_dat],
#' [AUTO_VI$plot_resid], [AUTO_VI$save_plot], [AUTO_VI$remove_plot],
#' [AUTO_VI$vss], [AUTO_VI$rotate_resid], [AUTO_VI$null_vss],
#' [AUTO_VI$boot_vss], [AUTO_VI$check], [AUTO_VI$lr_ratio],
#' [AUTO_VI$summary_plot]
#' @export
AUTO_VI <- new.env()

#' List of diagnostic results
#'
#' @name AUTO_VI$check_result
#'
#' @description A list, will be initialized after the method [AUTO_VI$check] is
#' run.
AUTO_VI$check_result

#' Initialization method
#'
#' @name AUTO_VI$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`.
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' @param keras_mod Keras model. A trained computer vision model. Default is
#' `NULL`.
#' @param dat Data frame. The data used to fit the model. Default is `NULL`.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes. Default is `1L`.
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
#' out of a model object by using [stats::fitted] and [stats::resid].
#' @param fitted_mod Model. A model object, e.g. `lm`. Default is
#' `self$fitted_mod`.
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
#' by using [stats::model.frame] if `self$dat` is `NULL`.
#' @param fitted_mod Model. A model object, e.g. `lm`. Default is
#' `self$fitted_mod`.
#' @return A tibble.
#'
#' @examples
#'
#' my_vi <- auto_vi(fitted_mod = lm(speed ~ dist, data = cars))
#' my_vi$get_dat()
AUTO_VI$get_dat

#' Draw a standard residual plot
#'
#' @name AUTO_VI$plot_resid
#'
#' @description This function draws a standard residual plot.
#' @param x Data frame. A data frame containing variables `.resid` and
#' `.fitted`. Default is `self$get_fitted_and_resid()`.
#' @param theme `ggtheme`. A `ggplot` theme object.
#' Default is `ggplot2::theme_light(base_size = 11/5)`.
#' @param alpha Numeric. Alpha of dot. Value between 0 and 1. Default is `1`.
#' @param size Numeric. Size of dot. Value between 0 and 1. Default is `0.5`.
#' @param stroke Numeric. Stroke of dot. Value between 0 and 1.
#' Default is `0.5`.
#' @param remove_axis Boolean. Whether or not to remove the axis.
#' Default is `TRUE`.
#' @param remove_legend Boolean. Whether or not to remove the legend.
#' Default is `TRUE`.
#' @param remove_grid_line Boolean. Whether or not to remove the grid lines.
#' Default is `TRUE`.
#' @param add_zero_line Boolean. Whether or not to add a zero horizontal line.
#' Default is `TRUE`.
#' @return A `ggplot`.
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
#' @description This function saves a plot with [ggplot2::ggsave].
#' @param p `ggplot`. A plot.
#' @param path Character. Path to save. Default is `tempfile(fileext = '.png')`.
#' @param width Numeric. Width of the plot. Default is `7/5`.
#' @param height Numeric. Height of the plot. Default is `7/4`.
#' @param ... Arguments passed to [ggplot2::ggsave].
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
#' @param p `ggplot`/List. A `ggplot` or a list of `ggplot`.
#' @param keras_mod Keras model. A trained computer vision model.
#' Default is `self$keras_mod`.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes. Default is `1L`.
#' @return A numeric vector.
AUTO_VI$vss

#' Get rotated residuals from a fitted linear model
#'
#' @name AUTO_VI$rotate_resid
#'
#' @description This function gets rotated residuals from a fitted linear
#' model. The rotated residuals are generated by first regressing random
#' noises on the original regressors, then multiply the obtained residuals by
#' original RSS divided by the current RSS. The results are the rotated
#' residuals.
#' @param fitted_mod `lm`. A linear model object. Default is
#' `self$fitted_mod`.
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
#' @param draws Integer. Number of simulation draws. Default is `100L`.
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' Default is `self$fitted_mod`.
#' @param keras_mod Keras model. A trained computer vision model.
#' Default is `self$keras_mod`.
#' @param method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid]. Default is `self$rotate_resid`.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes. Default is `1L`.
#' @param keep_null_dat Boolean. Whether to keep the simulated null data.
#' Default is `FALSE`.
#' @param keep_null_plot Boolean. Whether to keep the simulated null plots.
#' Default is `FALSE`.
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
#' @param draws Integer. Number of simulation draws. Default is `100L`.
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' Default is `self$fitted_mod`.
#' @param keras_mod Keras model. A trained computer vision model.
#' Default is `self$keras_mod`.
#' @param dat Data frame. The data used to fit the model.
#' Default is `self$get_dat()`.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes. Default is `1L`.
#' @param keep_boot_dat Boolean. Whether to keep the bootstrapped data.
#' Default is `FALSE`.
#' @param keep_boot_plot Boolean. Whether to keep the bootstrapped plots.
#' Default is `FALSE`.
#' @return A tibble with 1 to 3 columns depending on the argument
#' `keep_boot_dat` and `keep_boot_plot`.
AUTO_VI$boot_vss

#' Conduct a auto visual inference check with a computer vision model
#'
#' @name AUTO_VI$check
#'
#' @description This function conducts a visual inference
#' check with a computer vision model.
#'
#' @param null_draws Integer. Number of simulation draws for
#' [AUTO_VI$null_vss]. Default is `100L`.
#' @param boot_draws Integer. Number of simulation draws for
#' [AUTO_VI$boot_vss]. Default is `100L`.
#' @param fitted_mod Model. A model object, e.g. `lm`.
#' Default is `self$fitted_mod`.
#' @param keras_mod Keras model. A trained computer vision model.
#' Default is `self$keras_mod`.
#' @param method Function. A method to simulate residuals from the null
#' hypothesis distribution. For `lm`, the recommended method is residual
#' rotation [AUTO_VI$rotate_resid]. Default is `self$rotate_resid`.
#' @param dat Data frame. The data used to fit the model.
#' Default is `self$get_dat()`.
#' @param node_index Integer. An index indicating which node of the output layer
#' contains the visual signal strength. This is particularly useful
#' when the keras model has more than one output nodes. Default is `1L`.
#' @param keep_dat Boolean. Whether to keep the simulated data.
#' Default is `FALSE`.
#' @param keep_plot Boolean. Whether to keep the simulated plots.
#' Default is `FALSE`.
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
#' @return A named vector with three elements `boot`, `null` and `ratio`.
AUTO_VI$lr_ratio

#' Draw a summary plot for the result
#'
#' @name AUTO_VI$summary_plot
#'
#' @description This function draws a summary plot for the result.
#'
#' @param check_result List. The result. Default is `self$check_result`.
#' @return A `ggplot`.
AUTO_VI$summary_plot


