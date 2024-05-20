
# AUTO_VI -----------------------------------------------------------------


class_AUTO_VI <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- PC1 <- PC2 <- NULL

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
  # temp_dat <- tempfile(fileext = ".csv")
  # utils::write.csv(data.frame(fitted = dat$.fitted, resid = dat$.resid),
  #                  temp_dat)
  #
  # read_com <- paste0("x <- utils::read.csv(", temp_dat, ");")
  # cal_monotonic <- paste0("")
  # system2("Rscript", c("-e", "''"))

  measure_monotonic <- try_or_zero(cassowaryr::sc_monotonic, dat$.fitted, dat$.resid)
  measure_sparse <- try_or_zero(cassowaryr::sc_sparse2, dat$.fitted, dat$.resid)
  measure_splines <- try_or_zero(cassowaryr::sc_splines, dat$.fitted, dat$.resid)
  measure_striped <- try_or_zero(cassowaryr::sc_striped, dat$.fitted, dat$.resid)

  return(c(measure_monotonic = measure_monotonic,
           measure_sparse = measure_sparse,
           measure_splines = measure_splines,
           measure_striped = measure_striped,
           n = n))
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

    .fitted <- .resid <- NULL

    # The default arguments are what we used for training data preparation.
    if (add_zero_line) {
      p <- ggplot2::ggplot(dat) +
        ggplot2::geom_hline(yintercept = 0, col = "red") +
        ggplot2::geom_point(ggplot2::aes(.fitted, .resid),
                            alpha = alpha,
                            size = size,
                            stroke = stroke) +
        theme
    } else {
      p <- ggplot2::ggplot(dat) +
        ggplot2::geom_point(ggplot2::aes(.fitted, .resid),
                            alpha = alpha,
                            size = size,
                            stroke = stroke) +
        theme
    }

    if (remove_axis) {
      p <- p + ggplot2::theme(axis.line = ggplot2::element_blank(),
                              axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                              axis.text.y = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
                              axis.title.y = ggplot2::element_blank())
    }

    if (remove_legend) {
      p <- p + ggplot2::theme(legend.position = "none")
    }

    if (remove_grid_line) {
      p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                              panel.grid.minor = ggplot2::element_blank())
    }

    return(p)
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


# read_plot ---------------------------------------------------------------

  read_plot_ <- function(path, width, height) {
    autovi::check_python_library_available("PIL")
    autovi::check_python_library_available("tensorflow")
    PIL <- reticulate::import("PIL", convert = FALSE)
    tf <- reticulate::import("tensorflow", convert = TRUE)
    keras <- tf$keras

    # Load the image and resize it to the correct size.
    input_image <- PIL$Image$open(path)$resize(c(width, height))

    # Convert the image to a Numpy array.
    input_array <- keras$utils$img_to_array(input_image)

    return(input_array)
  }

# remove_plot -------------------------------------------------------------

  remove_plot_ <- function(path, check_ext = TRUE) {
    if (!file.exists(path)) return(warning("File doesn't exist!"))
    if (check_ext && (!tolower(tools::file_ext(path)) %in% c("tif", "tiff", "bmp", "jpg", "jpeg", "gif", "png", "eps")))
      stop(paste0("File extension `", tools::file_ext(path), "` does not apper to be associated with an image!"))

    file.remove(path)
    return(invisible(self))
  }


# predict_array -----------------------------------------------------------


  predict_array_ <- function(input_array,
                             auxiliary = NULL,
                             keras_mod = self$keras_mod,
                             node_index = self$node_index,
                             extract_feature_from_layer = NULL) {

    autovi::check_python_library_available("tensorflow")
    tf <- reticulate::import("tensorflow", convert = TRUE)
    keras <- tf$keras

    # Check if the keras model have multiple inputs
    mutltiple_inputs_flag <- length(keras_mod$inputs) > 1

    # Predict the batch.
    if (mutltiple_inputs_flag) {
      output <- keras_mod$`__call__`(list(input_array, auxiliary))$numpy()
    } else {
      output <- keras_mod$`__call__`(input_array)$numpy()
    }

    cli::cli_alert_success("Predict visual signal strength for {dim(input_array)[1]} image{?s}.")

    # Extract the value of a particular output node.
    if (!(is.vector(output) && is.atomic(output))) output <- output[, node_index]

    # Use the model as a feature extractor
    if (!is.null(extract_feature_from_layer)) {

      if(!(is.numeric(extract_feature_from_layer) || is.character(extract_feature_from_layer))) {
        stop("Argument `extract_feature_from_layer` needs to be an integer or a string.")
      }

      if (is.numeric(extract_feature_from_layer))
        target_layer <- keras_mod$get_layer(index = as.integer(extract_feature_from_layer) - 1L)

      if (is.character(extract_feature_from_layer))
        target_layer <- keras_mod$get_layer(extract_feature_from_layer)

      # Build a feature extraction model
      feature_mod <- keras$Model(inputs = keras_mod$input, outputs = target_layer$output)

      # Extract feature
      if (mutltiple_inputs_flag) {
        feature <- feature_mod$`__call__`(list(input_array, auxiliary))$numpy()
      } else {
        feature <- feature_mod$`__call__`(input_array)$numpy()
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


# predict_image -----------------------------------------------------------

  predict_image_ <- function(path,
                             auxiliary = NULL,
                             keras_mod = self$keras_mod,
                             node_index = self$node_index,
                             extract_feature_from_layer = NULL) {

    autovi::check_python_library_available("numpy")
    np <- reticulate::import("numpy", convert = FALSE)

    # Check if the keras model have multiple inputs
    mutltiple_inputs_flag <- length(keras_mod$inputs) > 1

    # Stop if `auxiliary` does not match the dimension of `path`.
    if (mutltiple_inputs_flag && nrow(auxiliary) != length(path)) {
      stop(paste0("The `auxiliary` needs to have ",
                  length(path),
                  " rows, but it has ",
                  nrow(auxiliary),
                  " rows!"))
    }

    # Get the input shape from the keras model.
    input_shape <- keras_mod$input_shape[[1]]
    height <- input_shape[[2]]
    width <- input_shape[[3]]

    # Init the input batch.
    input_batch <- vector(mode = "list", length = length(path))

    i <- 0
    for (this_path in path) {
      i <- i + 1
      input_array <- self$read_plot(path, width = width, height = height)

      # Store the array into the batch
      input_batch[[i]] <- input_array
    }

    # Convert a list of Numpy arrays to a batch.
    input_batch <- np$stack(input_batch)

    self$predict_array(input_batch,
                       auxiliary = auxiliary,
                       keras_mod = keras_mod,
                       node_index = node_index,
                       extract_feature_from_layer = extract_feature_from_layer)

  }


# predict_ggplot ----------------------------------------------------------

  predict_ggplot_ <- function(p,
                              auxiliary = NULL,
                              keras_mod = self$keras_mod,
                              node_index = self$node_index,
                              extract_feature_from_layer = NULL) {

    # Check if the keras model have multiple inputs
    mutltiple_inputs_flag <- length(keras_mod$inputs) > 1

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
    input_shape <- keras_mod$input_shape[[1]]
    height <- input_shape[[2]]
    width <- input_shape[[3]]

    # Init the input batch.
    input_batch <- vector(mode = "list", length = length(p_list))

    # Import necessary Python libraries
    autovi::check_python_library_available("PIL")
    autovi::check_python_library_available("numpy")
    autovi::check_python_library_available("tensorflow")
    PIL <- reticulate::import("PIL", convert = FALSE)
    np <- reticulate::import("numpy", convert = FALSE)
    tf <- reticulate::import("tensorflow", convert = TRUE)
    keras <- tf$keras

    # Init a temporary file for storing images.
    temp_path <- tempfile(fileext = ".png")

    # Init progress bar
    cli::cli_progress_bar("Preparing input images", total = length(p_list))

    i <- 0
    for (x in p_list) {
      i <- i + 1

      # Save the plot to the temporary file.
      self$save_plot(x, path = temp_path)

      input_array <- self$read_plot(temp_path, width = width, height = height)

      # Store the array into the batch
      input_batch[[i]] <- input_array

      # Update progress bar
      cli::cli_progress_update()
    }

    # Remove progress bar
    cli::cli_progress_done()

    # Convert a list of Numpy arrays to a batch.
    input_batch <- np$stack(input_batch)

    output <- self$predict_array(input_batch,
                                 auxiliary,
                                 keras_mod = keras_mod,
                                 node_index = node_index,
                                 extract_feature_from_layer = extract_feature_from_layer)

    # Clean up.
    self$remove_plot(temp_path)

    return(output)

  }

# vss ---------------------------------------------------------------------

  vss_ <- function(x = self$plot_resid(),
                   auxiliary = NULL,
                   keras_mod = self$keras_mod,
                   node_index = self$node_index,
                   extract_feature_from_layer = NULL) {

    # Check if the keras model have multiple inputs
    mutltiple_inputs_flag <- length(keras_mod$inputs) > 1

    # Decide if `auxiliary` is provided and if it is needed to be computed automatically.
    if (mutltiple_inputs_flag && is.null(auxiliary)) {
      auxiliary <- data.frame(as.list(self$auxiliary()))
    }

    # Decide the type of the input
    # A single ggplot
    if (ggplot2::is.ggplot(x)) {
      return(self$predict_ggplot(x,
                                 auxiliary = auxiliary,
                                 keras_mod = keras_mod,
                                 node_index = node_index,
                                 extract_feature_from_layer = extract_feature_from_layer))
    }

    # A list of ggplot
    if (is.list(x)) {
      if (all(unlist(lapply(x, ggplot2::is.ggplot)))) {
        return(self$predict_ggplot(x,
                                   auxiliary = auxiliary,
                                   keras_mod = keras_mod,
                                   node_index = node_index,
                                   extract_feature_from_layer = extract_feature_from_layer))
      }
    }

    # A data.frame
    if (is.data.frame(x)) {
      return(self$predict_ggplot(self$plot_resid(x),
                                 auxiliary = auxiliary,
                                 keras_mod = keras_mod,
                                 node_index = node_index,
                                 extract_feature_from_layer = extract_feature_from_layer))
    }

    # A 3D array
    if (is.array(x)) {
      if (length(dim(x)) == 3) {
        return(self$predict_array(x,
                                  auxiliary = auxiliary,
                                  keras_mod = keras_mod,
                                  node_index = node_index,
                                  extract_feature_from_layer = extract_feature_from_layer))
      }
    }

    # A path to an image
    if (is.character(x)) {
      return(self$predict_image(x,
                                auxiliary = auxiliary,
                                keras_mod = keras_mod,
                                node_index = node_index,
                                extract_feature_from_layer = extract_feature_from_layer))
    }

    # A list of paths to images
    if (is.list(x)) {
      if (all(unlist(lapply(x, is.character)))) {
        return(self$predict_image(x,
                                  auxiliary = auxiliary,
                                  keras_mod = keras_mod,
                                  node_index = node_index,
                                  extract_feature_from_layer = extract_feature_from_layer))
      }
    }
  }


# null_method -------------------------------------------------------------

  null_method_ <- function(fitted_mod = self$fitted_mod) {
    return(self$rotate_resid(fitted_mod))
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
                        null_method = self$null_method,
                        node_index = self$node_index,
                        keep_null_dat = FALSE,
                        keep_null_plot = FALSE,
                        extract_feature_from_layer = NULL) {

    if (draws <= 0) stop("Argument `draws` needs to be positive!")

    # Simulate null data.
    dat_list <- lapply(1:draws, function(i) null_method(fitted_mod))

    cli::cli_alert_success("Generate null data.")

    # Generate null plots.
    p_list <- lapply(dat_list, function(this_dat) self$plot_resid(this_dat))

    cli::cli_alert_success("Generate null plots.")

    # Calculate auxiliary data if needed.
    auxiliary <- NULL
    if (length(keras_mod$inputs) > 1) {

      # Init progress bar
      cli::cli_progress_bar("Computing auxiliary inputs", total = length(dat_list))

      auxiliary <- list()
      for (i in 1:length(dat_list)) {
        this_dat <- dat_list[[i]]
        auxiliary[[i]] <- self$auxiliary(this_dat)
        cli::cli_progress_update()
      }

      # Remove progress bar
      cli::cli_progress_done()

      auxiliary <- as.data.frame(t(as.data.frame(auxiliary)))
      rownames(auxiliary) <- NULL

      cli::cli_alert_success("Compute auxilary inputs.")
    }

    # Predict visual signal strength for these plots.
    vss <- self$vss(p_list,
                    auxiliary = auxiliary,
                    keras_mod = keras_mod,
                    node_index = node_index,
                    extract_feature_from_layer = extract_feature_from_layer)

    result <- vss

    if (keep_null_dat) result$dat <- dat_list
    if (keep_null_plot) result$plot <- p_list

    return(result)
  }

# boot_vss ----------------------------------------------------------------

  boot_vss_ <- function(draws = 100L,
                        fitted_mod = self$fitted_mod,
                        keras_mod = self$keras_mod,
                        dat = self$get_dat(),
                        node_index = self$node_index,
                        keep_boot_dat = FALSE,
                        keep_boot_plot = FALSE,
                        extract_feature_from_layer = NULL) {

    if (draws <= 0) stop("Argument `draws` needs to be positive!")

    # Bootstrap and refit regression models.
    dat_list <- lapply(1:draws, function(i) {

      # Sampling row ids with replacement.
      new_row_id <- sample(1:nrow(dat), replace = TRUE)

      # Refit the model.
      new_mod <- stats::update(fitted_mod, data = dat[new_row_id, ])

      tibble::tibble(.fitted = new_mod$fitted.values,
                     .resid = new_mod$residuals)
    })


    cli::cli_alert_success("Generate bootstrapped data.")

    # Generate null plots.
    p_list <- lapply(dat_list, function(this_dat) self$plot_resid(this_dat))

    cli::cli_alert_success("Generate bootstrapped plots.")

    # Calculate auxiliary data.
    auxiliary <- NULL
    if (length(keras_mod$inputs) > 1) {

      # Init progress bar
      cli::cli_progress_bar("Computing auxiliary inputs", total = length(dat_list))

      auxiliary <- list()
      for (i in 1:length(dat_list)) {
        this_dat <- dat_list[[i]]
        auxiliary[[i]] <- self$auxiliary(this_dat)
        cli::cli_progress_update()
      }

      # Remove progress bar
      cli::cli_progress_done()

      auxiliary <- as.data.frame(t(as.data.frame(auxiliary)))
      rownames(auxiliary) <- NULL

      cli::cli_alert_success("Compute auxilary inputs.")
    }

    # Predict visual signal strength for these plots.
    vss <- self$vss(p_list,
                    auxiliary = auxiliary,
                    keras_mod = keras_mod,
                    node_index = node_index,
                    extract_feature_from_layer = extract_feature_from_layer)

    result <- vss

    if (keep_boot_dat) result$dat <- dat_list
    if (keep_boot_plot) result$plot <- p_list

    return(result)
  }


# check -------------------------------------------------------------------

  check_ <- function(null_draws = 100L,
                     boot_draws = 100L,
                     fitted_mod = self$fitted_mod,
                     keras_mod = self$keras_mod,
                     null_method = self$null_method,
                     p_value_type = "quantile",
                     dat = self$get_dat(),
                     node_index = self$node_index,
                     keep_dat = FALSE,
                     keep_plot = FALSE,
                     extract_feature_from_layer = NULL) {

    if (null_draws <= 0) {
      null_dist <- NULL
    } else {
      # Get the null distribution.
      null_dist <- self$null_vss(null_draws,
                                 fitted_mod = fitted_mod,
                                 keras_mod = keras_mod,
                                 null_method = null_method,
                                 node_index = node_index,
                                 keep_null_dat = keep_dat,
                                 keep_null_plot = keep_plot,
                                 extract_feature_from_layer = extract_feature_from_layer)
    }

    if (boot_draws <= 0) {
      boot_dist <- NULL
    } else {
      # Get the bootstrapped distribution.
      boot_dist <- self$boot_vss(boot_draws,
                                 fitted_mod = fitted_mod,
                                 keras_mod = keras_mod,
                                 dat = dat,
                                 node_index = node_index,
                                 keep_boot_dat = keep_dat,
                                 keep_boot_plot = keep_plot,
                                 extract_feature_from_layer = extract_feature_from_layer)
    }

    # Get the observed visual signal strength.
    fitted_and_resid <- self$get_fitted_and_resid(fitted_mod = fitted_mod)
    p <- self$plot_resid(fitted_and_resid)
    observed <- self$vss(p,
                         keras_mod = keras_mod,
                         node_index = node_index,
                         extract_feature_from_layer = extract_feature_from_layer)

    # Store the results internally.
    self$check_result$null <- null_dist
    self$check_result$boot <- boot_dist

    self$check_result$observed <- observed

    # Compute the p-values.
    if (null_draws > 0)
      self$check_result$p_value <- self$p_value(observed$vss , p_value_type = p_value_type)
    if (null_draws > 0 && boot_draws > 0)
      self$check_result$boot_p_value <- self$p_value(mean(boot_dist$vss), p_value_type = p_value_type)

    # Compute the likelihoods and ratio.
    if (null_draws > 0 && boot_draws > 0) {
      lr_ratio <- self$lr_ratio()
      self$check_result$boot_likelihood <- lr_ratio["likelihood_1"]
      self$check_result$null_likelihood <- lr_ratio["likelihood_2"]
      self$check_result$lr_ratio <- lr_ratio["lr_ratio"]
    } else {
      self$check_result$boot_likelihood <- NULL
      self$check_result$null_likelihood <- NULL
      self$check_result$lr_ratio <- NULL
    }

    return(invisible(self))
  }



# lineup_check ------------------------------------------------------------

  lineup_check_ <- function(lineup_size = 20L,
                            fitted_mod = self$fitted_mod,
                            keras_mod = self$keras_mod,
                            null_method = self$null_method,
                            dat = self$get_dat(),
                            node_index = self$node_index,
                            extract_feature_from_layer = NULL) {

    self$check(null_draws = lineup_size - 1L,
               boot_draws = 0L,
               fitted_mod = fitted_mod,
               keras_mod = keras_mod,
               null_method = null_method,
               p_value_type = "lineup",
               dat = dat,
               node_index = node_index,
               keep_dat = TRUE,
               keep_plot = TRUE,
               extract_feature_from_layer = extract_feature_from_layer)
  }


# lr_ratio ----------------------------------------------------------------

  lr_ratio_ <- function(vss = self$check_result$observed$vss,
                        dist_1 = self$check_result$boot$vss,
                        dist_2 = self$check_result$null$vss) {

    if (is.null(vss)) stop("Missing observed visual signal strength!")
    if (is.null(dist_1)) stop("Missing results for distribution 1!")
    if (is.null(dist_2)) stop("Missing results for distribution 2!")

    # Extract the minimum and maximum to decide the boundary of density estimation.
    min_vss <- min(c(dist_2, dist_1, vss))
    max_vss <- max(c(dist_2, dist_1, vss))

    # Estimate the null density and bootstrapped density.
    den_1 <- stats::density(dist_1, from = min_vss, to = max_vss)
    den_2 <- stats::density(dist_2, from = min_vss, to = max_vss)

    # Approximate the likelihood of observing the visual signal strength
    # from the null null distribution and bootstrapped distribution.
    approx_1 <- stats::approx(den_1$x, den_1$y, vss)$y
    approx_2 <- stats::approx(den_2$x, den_2$y, vss)$y

    return(c(likelihood_1 = approx_1,
             likelihood_2 = approx_2,
             lr_ratio = approx_1/approx_2))
  }


# p_value -----------------------------------------------------------------

  p_value_ <- function(vss = self$check_result$observed$vss,
                       null_dist = self$check_result$null$vss,
                       p_value_type = "quantile") {

    if (is.null(vss)) stop("Missing observed visual signal strength!")
    if (is.null(null_dist)) stop("Missing results for null distribution!")

    if (p_value_type == "quantile") {
      return(mean(null_dist >= vss))
    }

    if (p_value_type == "lineup") {
      total <- length(null_dist) + 1
      return(1/total + sum(null_dist > vss)/total)
    }

    stop("Argument `p_value_type` is neither 'quantile' nor 'lineup'!")
  }


# summary_density_plot ----------------------------------------------------

  summary_density_plot_ <- function(vss = self$check_result$observed$vss,
                                    null_dist = self$check_result$null$vss,
                                    boot_dist = self$check_result$boot$vss,
                                    p_value = self$check_result$p_value,
                                    lr_ratio = self$check_result$lr_ratio,
                                    density_alpha = 0.6) {

    if (is.null(vss)) stop("Missing observed visual signal strength!")

    p <- ggplot2::ggplot()

    if (!is.null(null_dist)) p <- p + ggplot2::geom_density(ggplot2::aes(null_dist, fill = "Null", col = "Null"), alpha = density_alpha)
    if (!is.null(boot_dist)) p <- p + ggplot2::geom_density(ggplot2::aes(boot_dist, fill = "Boot", col = "Boot"), alpha = density_alpha)

    p <- p + ggplot2::geom_segment(ggplot2::aes(x = vss,
                                                xend = vss,
                                                y = 0,
                                                yend = Inf,
                                                linetype = "Observed vss"))

    if (!is.null(null_dist)) p <- p + ggplot2::geom_segment(ggplot2::aes(x = stats::quantile(null_dist, c(0.95)),
                                                                                             xend = stats::quantile(null_dist, c(0.95)),
                                                                                             y = 0,
                                                                                             yend = Inf,
                                                                                             linetype = "95% quantile of the null distribution"))

    p <- p + ggplot2::xlab("Visual signal strength") +
      ggplot2::ylab("Density") +
      ggplot2::labs(fill = "", color = "") +
      ggplot2::theme_light()

    subtitle <- ""
    if (!is.null(p_value)) subtitle <- paste0("P-value = ", format(p_value, digits = 4))
    if (!is.null(lr_ratio)) subtitle <- paste0(subtitle, ", Likelihood ratio = ", format(lr_ratio, digits = 4))

    p <- p + ggplot2::ggtitle("Summary of check result (density)", subtitle = subtitle)

    return(p)
  }


# summary_rank_plot -------------------------------------------------------

  summary_rank_plot_ <- function(vss = self$check_result$observed$vss,
                                 null_dist = self$check_result$null$vss,
                                 p_value = self$check_result$p_value) {

    if (is.null(vss)) stop("Missing observed visual signal strength!")
    if (is.null(null_dist)) stop("Missing results for null distribution!")

    y <- c(vss, null_dist)
    x <- length(y) - rank(y, ties.method = "first") + 1

    p <- ggplot2::ggplot() +
      ggplot2::geom_col(data = NULL, ggplot2::aes(x, y)) +
      ggplot2::geom_col(data = NULL, ggplot2::aes(x[1],
                                                  y[1],
                                                  fill = "observed",
                                                  col = "observed")) +
      ggplot2::xlab("Rank") +
      ggplot2::ylab("Visual signal strength") +
      ggplot2::theme_light()

    subtitle <- ""
    if (!is.null(p_value)) subtitle <- paste0("P-value = ", format(p_value, digits = 4))

    p <- p + ggplot2::ggtitle("Summary of check result (rank)", subtitle = subtitle)

    return(p)
  }

# summary_plot ------------------------------------------------------------

  summary_plot_ <- function(type = "density") {
    if (type == "density") return(self$summary_density_plot())
    if (type == "rank") return(self$summary_rank_plot())
    stop("Argument `type` is neither 'density' nor 'rank'!")
  }


# feature_pca -------------------------------------------------------------

  feature_pca_ <- function(feature = self$check_result$observed[, grep("f_", names(self$check_result$observed))],
                           null_feature = self$check_result$null[, grep("f_", names(self$check_result$null))],
                           boot_feature = self$check_result$boot[, grep("f_", names(self$check_result$boot))],
                           center = TRUE,
                           scale = TRUE) {

    all_feature <- data.frame()
    tags <- c()

    if (!is.null(feature)) {
      all_feature <- rbind(all_feature, feature)
      tags <- c(tags, "observed")
    }

    if (!is.null(null_feature)) {
      all_feature <- rbind(all_feature, null_feature)
      tags <- c(tags, rep("null", nrow(null_feature)))
    }

    if (!is.null(boot_feature)) {
      all_feature <- rbind(all_feature, boot_feature)
      tags <- c(tags, rep("boot", nrow(boot_feature)))
    }

    if (nrow(all_feature) == 0) stop("Can not find any feautre!")

    combined_feature <- all_feature
    combined_feature$set <- tags

    # Drop columns with no variance
    normal_feature_index <- c()

    for (i in 1:ncol(all_feature)) {
      if (stats::sd(all_feature[[i]]) > 0) {
        normal_feature_index <- c(normal_feature_index, i)
      }
    }

    if (length(normal_feature_index) == 0) stop("All features have zero variance!")

    all_feature <- all_feature[, normal_feature_index]

    pca <- stats::prcomp(all_feature, center = center, scale. = scale)

    combined_feature <- cbind(combined_feature, as.data.frame(pca$x))
    combined_feature <- tibble::as_tibble(combined_feature)

    attr(combined_feature, "sdev") <- pca$sdev
    attr(combined_feature, "rotation") <- pca$rotation
    return(combined_feature)
  }

# feature_plot ------------------------------------------------------------

  feature_plot_ <- function(feature_pca = self$feature_pca(),
                            x = PC1,
                            y = PC2,
                            col_by_set = TRUE) {
    set <- NULL

    if (col_by_set) {
      p <- ggplot2::ggplot(feature_pca) +
        ggplot2::geom_point(ggplot2::aes({{x}}, {{y}}, col = set))
    } else {
      p <- ggplot2::ggplot(feature_pca) +
        ggplot2::geom_point(ggplot2::aes({{x}}, {{y}}))
    }

    return(p)
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
      input_shape <- paste(unlist(self$keras_mod$input_shape[[1]]), collapse = ", ")
      output_shape <- paste(unlist(self$keras_mod$output_shape), collapse = ", ")

      if (length(self$keras_mod$inputs) == 1) {
        keras_mod_status <- paste0("(None, ", input_shape, ") -> ", "(None, ", output_shape, ")")
      } else {
        second_input_shape <- unlist(self$keras_mod$input_shape[[2]])
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
      return(result)
    }



    result <- paste0(result, "\n  - Result:")
    result <- paste0(result, "\n     - Observed visual signal strength: ",
                     format(self$check_result$observed$vss, digits = 4))

    # Get the null p-value.
    p_value <- self$check_result$p_value
    if (!is.null(p_value)) result <- paste0(result, " (p-value = ", format(p_value, digits = 4), ")")

    # Report the mean and the quantiles of the null distribution.
    null_dist <- self$check_result$null$vss
    if (!is.null(null_dist)) {
      qts <- stats::quantile(null_dist, c(0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99))
      qts <- utils::capture.output(print(qts, digits = 4))
      result <- paste0(result, "\n     - Null visual signal strength: [", length(null_dist), " draws]")
      result <- paste0(result, "\n        - Mean: ", format(mean(null_dist), digits = 4))
      result <- paste0(result, "\n        - Quantiles: ")
      result <- paste0(result, "\n           \u2554", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u2557")
      result <- paste0(result, "\n           \u2551", qts[1], "\u2551")
      result <- paste0(result, "\n           \u2551", qts[2], "\u2551")
      result <- paste0(result, "\n           \u255A", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u255D")
    }


    boot_dist <- self$check_result$boot$vss
    if (!is.null(boot_dist)) {

      # Report the mean and the quantiles of the bootstrapped distribution.
      qts <- stats::quantile(boot_dist, c(0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99))
      qts <- utils::capture.output(print(qts, digits = 4))
      result <- paste0(result, "\n     - Bootstrapped visual signal strength: [", length(boot_dist), " draws]")
      result <- paste0(result, "\n        - Mean: ", format(mean(boot_dist), digits = 4))

      # Get the boot p-value.
      boot_p_value <- self$check_result$boot_p_value
      if (!is.null(boot_p_value)) result <- paste0(result, " (p-value = ", boot_p_value, ")")

      result <- paste0(result, "\n        - Quantiles: ")
      result <- paste0(result, "\n           \u2554", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u2557")
      result <- paste0(result, "\n           \u2551", qts[1], "\u2551")
      result <- paste0(result, "\n           \u2551", qts[2], "\u2551")
      result <- paste0(result, "\n           \u255A", paste(rep("\u2550", nchar(qts[1])), collapse = ""), "\u255D")
    }


    # Report the likelihood ratio.
    lr_ratio <- self$check_result$lr_ratio

    if (!is.null(lr_ratio)) {
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
                             read_plot = read_plot_,
                             remove_plot = remove_plot_,
                             predict_array = predict_array_,
                             predict_image = predict_image_,
                             predict_ggplot = predict_ggplot_,
                             vss = vss_,
                             null_method = null_method_,
                             rotate_resid = rotate_resid_,
                             null_vss = null_vss_,
                             boot_vss = boot_vss_,
                             check = check_,
                             lineup_check = lineup_check_,
                             lr_ratio = lr_ratio_,
                             p_value = p_value_,
                             summary_density_plot = summary_density_plot_,
                             summary_rank_plot = summary_rank_plot_,
                             summary_plot = summary_plot_,
                             feature_pca = feature_pca_,
                             feature_plot = feature_plot_,
                             ..str.. = str_)
}

