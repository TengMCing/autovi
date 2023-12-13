
# AUTO_VI -----------------------------------------------------------------


class_AUTO_VI <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  bandicoot::new_class(bandicoot::BASE, env = env, class_name = "AUTO_VI")

  # A list for storing the result of `self$check()`.
  env$check_result <- list()

# init --------------------------------------------------------------------

  init_ <- function(fitted_mod, keras_mod = NULL, dat = NULL, node_index = 1L) {

    self$fitted_mod <- fitted_mod
    self$keras_mod <- keras_mod
    self$dat <- dat
    self$node_index <- node_index

    return(invisible(self))
  }


# get_fitted_and_resid ----------------------------------------------------

  get_fitted_and_resid_ <- function(fitted_mod = self$fitted_mod) {

    # This method fully relies on the S3 methods being defined
    tibble::tibble(.fitted = stats::fitted(fitted_mod),
                   .resid = stats::resid(fitted_mod))
  }

# get_dat -----------------------------------------------------------------

  get_dat_ <- function(fitted_mod = self$fitted_mod) {
    if (!is.null(self$dat)) return(self$dat)

    # Is this a reliable method to extract the data from a fit?
    return(stats::model.frame(fitted_mod))
  }


# auxiliary ---------------------------------------------------------------

  auxiliary_ <- function(dat = self$get_fitted_and_resid()) {

    n <- nrow(dat)

    try_or_zero <- function(fn, ...) {
      try_result <- try(fn(...), silent = TRUE)
      if (inherits(try_result, "try-error")) return(0)
      return(ifelse(is.na(try_result), 0, try_result))
    }

    # Only these scagnostics work.
    # Other measures will crash R so we did not train the CV model for them.
    # (13/12/2023)
    measure_monotonic <- try_or_zero(cassowaryr::sc_monotonic, dat$.fitted, dat$.resid)
    measure_sparse <- try_or_zero(cassowaryr::sc_sparse2, dat$.fitted, dat$.resid)
    measure_splines <- try_or_zero(cassowaryr::sc_splines, dat$.fitted, dat$.resid)
    measure_striped <- try_or_zero(cassowaryr::sc_striped, dat$.fitted, dat$.resid)

    return(c(n = n,
             measure_monotonic = measure_monotonic,
             measure_sparse = measure_sparse,
             measure_splines = measure_splines,
             measure_striped = measure_striped))
  }

# plot_resid --------------------------------------------------------------


  plot_resid_ <- function(dat = self$get_fitted_and_resid(),
                          theme = ggplot2::theme_light(base_size = 11/5),
                          alpha = 1,
                          size = 0.5,
                          stroke = 0.5,
                          remove_axis = TRUE,
                          remove_legend = TRUE,
                          remove_grid_line = TRUE,
                          add_zero_line = TRUE) {

    # The default arguments are what we used for training data preparation.
    visage::VI_MODEL$plot(dat,
                          theme = theme,
                          alpha = alpha,
                          size = size,
                          stroke = stroke,
                          remove_axis = remove_axis,
                          remove_legend = remove_legend,
                          remove_grid_line = remove_grid_line,
                          add_zero_line = add_zero_line)
  }

# save_plot ---------------------------------------------------------------

  save_plot_ <- function(p,
                         path = tempfile(fileext = ".png"),
                         width = 7/5,
                         height = 7/4,
                         ...) {
    ggplot2::ggsave(path, plot = p, width = width, height = height, ...)
    return(path)
  }


# remove_plot -------------------------------------------------------------

  remove_plot_ <- function(path, check_ext = TRUE) {
    if (!file.exists(path)) return(warning("File doesn't exist!"))
    if (check_ext && (!tolower(tools::file_ext(path)) %in% c("tif", "tiff", "bmp", "jpg", "jpeg", "gif", "png", "eps")))
      stop(paste0("File extension `", tools::file_ext(path), "` does not apper to be associated with an image!"))

    file.remove(path)
    return(invisible(self))
  }

# vss ---------------------------------------------------------------------

  vss_ <- function(p = self$plot_resid(),
                   auxiliary = NULL,
                   keras_mod = self$keras_mod,
                   node_index = self$node_index) {

    # Check if the keras model have multiple inputs
    mutltiple_inputs_flag <- length(keras_mod$inputs) > 1

    # Decide if `auxiliary` is provided and if it is needed to be computed automatically.
    if (mutltiple_inputs_flag && is.null(auxiliary)) {
      auxiliary <- self$auxiliary()
    }

    # Decide if the input is a list of plots or a single plot.
    if (ggplot2::is.ggplot(p)) {
      p_list <- list(p)
    } else {
      p_list <- p
    }

    # Stop if `auxiliary` does not match the dimension of `p_list`.
    if (mutltiple_inputs_flag && nrow(auxiliary) != length(p_list)) {
      stop(paste0("The `auxiliary` needs to have ",
                  length(p_list),
                  " rows, but it has ",
                  nrow(auxiliary),
                  " rows!"))
    }

    # Get the input shape from the keras model.
    input_shape <- keras_mod$layers[[1]]$input_shape[[1]]
    height <- input_shape[[2]]
    width <- input_shape[[3]]

    # Init the input batch.
    input_batch <- vector(mode = "list", length = length(p_list))

    # Import necessary Python libraries
    PIL <- reticulate::import("PIL", convert = FALSE)
    np <- reticulate::import("numpy", convert = FALSE)

    # Init a temporary file for storing images.
    temp_path <- tempfile(fileext = ".png")

    i <- 0
    for (x in p_list) {
      i <- i + 1

      # Save the plot to the temporary file.
      self$save_plot(x, path = temp_path)

      # Load the image and resize it to the correct size.
      input_image <- PIL$Image$open(temp_path)$resize(c(width, height))

      # Convert the image to a Numpy array.
      input_array <- keras::image_to_array(input_image)

      # Store the array into the batch
      input_batch[[i]] <- input_array
    }

    # Convert a list of Numpy arrays to a batch.
    input_batch <- np$stack(input_batch)

    # Predict the batch.
    if (mutltiple_inputs_flag) {
      output <- keras_mod$predict(list(input_batch, auxiliary), verbose = 0L)
    } else {
      output <- keras_mod$predict(input_batch, verbose = 0L)
    }

    # Clean up.
    self$remove_plot(temp_path)

    # Extract the value of a particular output node.
    return(output[, node_index])

  }


# rotate_resid ------------------------------------------------------------

  rotate_resid_ <- function(fitted_mod = self$fitted_mod) {
    if (!"lm" %in% class(fitted_mod)) stop("This function only supports `lm` model!")

    # Get the original data.
    ori_dat <- stats::model.frame(fitted_mod)

    # Replace the response variable with some values simulated from the
    # standard normal distribution.
    ori_dat[[1]] <- stats::rnorm(length(fitted_mod$residuals))

    # Refit the model.
    new_mod <- stats::update(fitted_mod, data = ori_dat)

    # Calculate the RSS ratio.
    rss_ratio <- sqrt(sum(fitted_mod$residuals^2)/sum(new_mod$residuals^2))

    # Scale the rotated residuals.
    return(tibble::tibble(.fitted = fitted_mod$fitted.values,
                          .resid = new_mod$residuals * rss_ratio))
  }


# null_vss ----------------------------------------------------------------

  null_vss_ <- function(draws = 100L,
                        fitted_mod = self$fitted_mod,
                        keras_mod = self$keras_mod,
                        null_method = self$rotate_resid,
                        node_index = self$node_index,
                        keep_null_dat = FALSE,
                        keep_null_plot = FALSE) {

    # Simulate null data.
    dat_list <- lapply(1:draws, function(i) null_method(fitted_mod))

    # Generate null plots.
    p_list <- lapply(dat_list, function(this_dat) self$plot_resid(this_dat))

    # Calculate auxiliary data if needed.
    auxiliary <- NULL
    if (length(keras_mod$inputs) > 1) {
      auxiliary <- lapply(dat_list, function(this_dat) self$auxiliary(this_dat))
      auxiliary <- as.data.frame(t(as.data.frame(auxiliary)))
      rownames(auxiliary) <- NULL
    }

    # Predict visual signal strength for these plots.
    vss <- self$vss(p_list,
                    auxiliary = auxiliary,
                    keras_mod = keras_mod,
                    node_index = node_index)

    result <- tibble::tibble(vss = vss)

    if (keep_null_dat) result$dat <- dat_list
    if (keep_null_plot) result$plot <- p_list

    return(result)
  }


# unique_obs_correction ---------------------------------------------------

  unique_obs_correction_ <- function(vss, overlap_ratio) {
    log((exp(vss) - 1) / overlap_ratio + 1)
  }

# boot_vss ----------------------------------------------------------------

  boot_vss_ <- function(draws = 100L,
                        fitted_mod = self$fitted_mod,
                        keras_mod = self$keras_mod,
                        correction = FALSE,
                        jitter = FALSE,
                        factor = 1L,
                        dat = self$get_dat(),
                        node_index = 1L,
                        keep_boot_dat = FALSE,
                        keep_boot_plot = FALSE) {

    # Decide between jitter and sampling with replacement.
    if (jitter) {
      dat_list <- lapply(1:draws, function(i) {

        # Prepare the data for fitting.
        new_dat <- dat

        # Jitter each column.
        for (i in 1:ncol(dat)) {
          if (i == 1) next
          new_dat[[i]] <- jitter(new_dat[[i]], factor = factor)
        }

        # Refit the model.
        new_mod <- stats::update(fitted_mod, data = new_dat)
        tibble::tibble(.fitted = new_mod$fitted.values,
                       .resid = new_mod$residuals,
                       .overlap_ratio = NA)
      })
    } else {
      dat_list <- lapply(1:draws, function(i) {

        # Sampling row ids with replacement.
        new_row_id <- sample(1:nrow(dat), replace = TRUE)

        # Refit the model.
        new_mod <- stats::update(fitted_mod, data = dat[new_row_id, ])
        tibble::tibble(.fitted = new_mod$fitted.values,
                       .resid = new_mod$residuals,
                       .overlap_ratio = length(intersect(1:nrow(dat), new_row_id))/nrow(dat))
      })
    }

    # Generate null plots.
    p_list <- lapply(dat_list, function(this_dat) self$plot_resid(this_dat))

    # Calculate auxiliary data.
    auxiliary <- NULL
    if (length(keras_mod$inputs) > 1) {
      auxiliary <- lapply(dat_list, function(this_dat) self$auxiliary(this_dat))
      auxiliary <- as.data.frame(t(as.data.frame(auxiliary_dat)))
      rownames(auxiliary_dat) <- NULL
    }

    # Predict visual signal strength for these plots.
    vss <- self$vss(p_list,
                    auxiliary = auxiliary,
                    keras_mod = keras_mod,
                    node_index = node_index)

    # Apply unique ratio correction.
    if (correction && !jitter) {
      overlap_ratio <- unlist(lapply(dat_list, function(this_dat) this_dat$.overlap_ratio))
      vss <- self$unique_obs_correction(vss, overlap_ratio)
    }

    result <- tibble::tibble(vss = vss)

    if (keep_boot_dat) result$dat <- dat_list
    if (keep_boot_plot) result$plot <- p_list

    return(result)
  }


# check -------------------------------------------------------------------

  check_ <- function(null_draws = 100L,
                     boot_draws = 100L,
                     fitted_mod = self$fitted_mod,
                     keras_mod = self$keras_mod,
                     null_method = self$rotate_resid,
                     correction = FALSE,
                     jitter = FALSE,
                     factor = 1L,
                     dat = self$get_dat(),
                     node_index = self$node_index,
                     keep_dat = FALSE,
                     keep_plot = FALSE) {

    # Get the null distribution.
    null_dist <- self$null_vss(null_draws,
                               fitted_mod = fitted_mod,
                               keras_mod = keras_mod,
                               null_method = null_method,
                               node_index = node_index,
                               keep_null_dat = keep_dat,
                               keep_null_plot = keep_plot)

    # Get the bootstrapped distribution.
    boot_dist <- self$boot_vss(boot_draws,
                               fitted_mod = fitted_mod,
                               keras_mod = keras_mod,
                               correction = correction,
                               jitter = jitter,
                               factor = factor,
                               dat = dat,
                               node_index = node_index,
                               keep_boot_dat = keep_dat,
                               keep_boot_plot = keep_plot)

    # Get the observed visual signal strength.
    fitted_and_resid <- self$get_fitted_and_resid(fitted_mod = fitted_mod)
    p <- self$plot_resid(fitted_and_resid)
    observed_vss <- self$vss(p, keras_mod = keras_mod, node_index = node_index)

    # Store the results internally.
    self$check_result$null <- null_dist
    self$check_result$boot <- boot_dist
    self$check_result$vss <- observed_vss

    # Compute the p-values.
    self$check_result$p_value <-  self$p_value(type = "null")
    self$check_result$boot_p_value <- self$p_value(type = "boot")

    # Compute the likelihoods and ratio.
    lr_ratio <- self$lr_ratio()
    self$check_result$boot_likelihood <- lr_ratio$boot_likelihood
    self$check_result$null_likelihood <- lr_ratio$null_likelihood
    self$check_result$lr_ratio <- lr_ratio$lr_ratio

    return(invisible(self))
  }


# lr_ratio ----------------------------------------------------------------

  lr_ratio_ <- function() {

    # If `self$check_result` is empty, returns NA.
    if (length(self$check_result) == 0) return(NA)

    # Extract all the visual signal strengths.
    null_vss <- self$check_result$null$vss
    boot_vss <- self$check_result$boot$vss
    observed <- self$check_result$vss

    # Extract the minimum and maximum to decide the boundary of density estimation.
    min_vss <- min(c(null_vss, boot_vss, observed))
    max_vss <- max(c(null_vss, boot_vss, observed))

    # Estimate the null density and bootstrapped density.
    null_den <- stats::density(null_vss, from = min_vss, to = max_vss)
    boot_den <- stats::density(boot_vss, from = min_vss, to = max_vss)

    # Approximate the likelihood of observing the visual signal strength
    # from the null null distribution and bootstrapped distribution.
    null_approx <- stats::approx(null_den$x, null_den$y, observed)$y
    boot_approx <- stats::approx(boot_den$x, boot_den$y, observed)$y

    # Calculate likelihood ratio.
    result <- boot_approx/null_approx

    return(c(boot_likelihood = boot_approx,
             null_likelihood = null_approx,
             lr_ratio = boot_approx/null_approx))
  }


# p_value -----------------------------------------------------------------

  p_value_ <- function(type = "null") {

    # If `self$check_result` is empty, returns NA.
    if (length(self$check_result) == 0) return(NA)

    # This is comparing the observed visual signal strength with the
    # null distribution.
    if (type == "null") {
      return(mean(self$check_result$null$vss >= self$check_result$vss))
    }

    # This is comparing the mean of the bootstrapped distribution with
    # the null distribution.
    if (type == "boot") {
      return(mean(self$check_result$null$vss >= mean(self$check_result$boot$vss)))
    }

    stop("Argument `type` is neither 'null' nor 'boot'!")
  }

# # lr_ratio_status ---------------------------------------------------------
#
#   lr_ratio_status_ <- function(ratio = self$lr_ratio()) {
#     if (is.nan(ratio)) return("The bootstrapped distribution does not capture the observed value!")
#     if (is.infinite(ratio)) return("It is very unlikely the observed value is from the null VSS distribution!")
#   }

# summary_plot ------------------------------------------------------------

  summary_plot_ <- function() {
    ggplot2::ggplot() +
      ggplot2::geom_density(ggplot2::aes(self$check_result$null$vss, fill = "Null", col = "Null"), alpha = 0.6) +
      ggplot2::geom_density(ggplot2::aes(self$check_result$boot$vss, fill = "Boot", col = "Boot"), alpha = 0.6) +
      ggplot2::geom_segment(ggplot2::aes(x = self$check_result$vss,
                                         xend = self$check_result$vss,
                                         y = 0,
                                         yend = Inf,
                                         linetype = "Observed vss")) +
      ggplot2::geom_segment(ggplot2::aes(x = stats::quantile(self$check_result$null$vss, c(0.95)),
                                         xend = stats::quantile(self$check_result$null$vss, c(0.95)),
                                         y = 0,
                                         yend = Inf,
                                         linetype = "95% quantile of the null distribution")) +
      ggplot2::xlab("Visual signal strength") +
      ggplot2::ylab("Density") +
      ggplot2::labs(fill = "", color = "") +
      ggplot2::theme_light() +
      ggplot2::ggtitle("Summary of check result",
                       subtitle = paste0("P-value = ",
                                         format(self$check_result$p_value, digits = 4),
                                         ", Likelihood ratio = ",
                                         format(self$check_result$lr_ratio, digits = 4)))
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

    # Report the fitted model.
    if (is.null(self$fitted_mod)) {
      fitted_mod_status <- "UNKNOWN"
    } else {
      fitted_mod_status <- paste(class(self$fitted_mod), collapse = ", ")
    }
    result <- paste0(result, "\n  - Fitted model: ", fitted_mod_status)

    # Report the keras model.
    if (is.null(self$keras_mod)) {
      keras_mod_status <- "UNKNOWN"
    } else {
      input_shape <- paste(unlist(self$keras_mod$layers[[1]]$input_shape[[1]]), collapse = ", ")
      output_shape <- paste(unlist(self$keras_mod$output_shape), collapse = ", ")

      if (length(self$keras_mod$inputs) == 1) {
        keras_mod_status <- paste0("(None, ", input_shape, ") -> ", "(None, ", output_shape, ")")
      } else {
        second_input_shape <- self$keras_mod$inputs[[2]]$shape[[2]]
        keras_mod_status <- paste0("(None, ", input_shape, ") + (None, ", second_input_shape, ") -> ", "(None, ", output_shape, ")")
      }

    }
    result <- paste0(result, "\n  - Keras model: ", keras_mod_status)

    # Report the output node of the keras model.
    if (!is.null(self$node_index)) {
      result <- paste0(result, "\n     - Output node index: ", self$node_index)
    }

    # Report the check result.
    if (length(self$check_result) == 0) {
      result <- paste0(result, "\n  - Result: UNKNOWN")
    } else {

      # Get the null p-value.
      p_value <- self$check_result$p_value
      result <- paste0(result, "\n  - Result:")
      result <- paste0(result, "\n     - Observed visual signal strength: ",
                       format(self$check_result$vss, digits = 4),
                       " (p-value = ", format(p_value, digits = 4), ")")

      # Report the mean and the quantiles of the null distribution.
      qts <- stats::quantile(self$check_result$null$vss, c(0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99))
      qts <- utils::capture.output(print(qts, digits = 4))
      result <- paste0(result, "\n     - Null visual signal strength: [", nrow(self$check_result$null), " draws]")
      result <- paste0(result, "\n        - Mean: ", format(mean(self$check_result$null$vss), digits = 4))
      result <- paste0(result, "\n        - Quantiles: ")
      result <- paste0(result, "\n           \u2554", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u2557")
      result <- paste0(result, "\n           \u2551", qts[1], "\u2551")
      result <- paste0(result, "\n           \u2551", qts[2], "\u2551")
      result <- paste0(result, "\n           \u255A", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u255D")

      # Get the boot p-value.
      p_value <- self$check_result$boot_p_value

      # Report the mean and the quantiles of the bootstrapped distribution.
      qts <- stats::quantile(self$check_result$boot$vss, c(0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99))
      qts <- utils::capture.output(print(qts, digits = 4))
      result <- paste0(result, "\n     - Bootstrapped visual signal strength: [", nrow(self$check_result$boot), " draws]")
      result <- paste0(result, "\n        - Mean: ", format(mean(self$check_result$boot$vss), digits = 4), " (p-value = ", p_value, ")")
      result <- paste0(result, "\n        - Quantiles: ")
      result <- paste0(result, "\n           \u2554", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u2557")
      result <- paste0(result, "\n           \u2551", qts[1], "\u2551")
      result <- paste0(result, "\n           \u2551", qts[2], "\u2551")
      result <- paste0(result, "\n           \u255A", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u255D")

      # Report the likelihood ratio.
      lr_ratio <- self$check_result$lr_ratio

      # Apply special treatments.
      if (is.infinite(lr_ratio)) {
        lr_ratio <- "Extremely large"
      } else if (lr_ratio == 0) {
        lr_ratio <- "Extremely small"
      }

      result <- paste0(result, "\n     - Likelihood ratio: ",
                       format(self$check_result$boot_likelihood, digits = 4),
                       " (boot) / ",
                       format(self$check_result$null_likelihood, digits = 4),
                       " (null) = ",
                       format(lr_ratio, digits = 4))

    }

    return(result)
  }

  bandicoot::register_method(env,
                             ..init.. = init_,
                             get_fitted_and_resid = get_fitted_and_resid_,
                             get_dat = get_dat_,
                             auxiliary = auxiliary_,
                             plot_resid = plot_resid_,
                             save_plot = save_plot_,
                             remove_plot = remove_plot_,
                             vss = vss_,
                             rotate_resid = rotate_resid_,
                             null_vss = null_vss_,
                             unique_obs_correction = unique_obs_correction_,
                             boot_vss = boot_vss_,
                             check = check_,
                             lr_ratio = lr_ratio_,
                             p_value = p_value_,
                             summary_plot = summary_plot_,
                             ..str.. = str_)
}

