# Re-export some bandicoot functions --------------------------------------

#' @export
#' @importFrom bandicoot new_class
bandicoot::new_class

#' @export
#' @importFrom bandicoot register_method
bandicoot::register_method

# check_python_library_available ------------------------------------------

#' Check python library availability
#'
#' This function checks if a python library is available. If the library can
#' not be found by the `importlib.util.find_spec` method, then an
#' error will be throw.
#'
#' @param lib_name Character. A library name.
#' @return No return. Called for side-effect.
#' @examples
#' try(check_python_library_available("numpy"))
#' @export
check_python_library_available <- function(lib_name) {
  if (!reticulate::py_available(initialize = TRUE)) stop("Python is not available on this system!")
  find_spec <- reticulate::import("importlib.util", convert = FALSE)$find_spec
  module_spec <- find_spec(lib_name)

  if (is.null(reticulate::py_to_r(module_spec)))
    stop(paste0("Library `", lib_name, "` can not be found in the currently used version of Python! Consider using `reticulate::py_config()` to confirm the version of Python is correct and using `reticulate::use_python()` to select a Python interpreter."))

  return(invisible(NULL))
}


# save_plot ---------------------------------------------------------------

#' Save plot(s)
#'
#' This function save a plot of a list of plots to provided path(s).
#'
#' @param p `ggplot`. A plot.
#' @param path Character. Path(s) to save the image.
#' @param width Numeric. Width of the image.
#' @param height Numeric. Height of the image.
#' @param ... Other arguments passed to [ggplot2::ggsave()].
#' @return The image path(s).
#' @examples
#' p <- ggplot2::ggplot(cars) + ggplot2::geom_point(ggplot2::aes(dist, speed))
#' save_plot(p)
#'
#' @export
save_plot <- function(p,
                      path = NULL,
                      width = 7/5,
                      height = 7/4,
                      ...) {

  # If a list of plots provided
  if (is.list(p)) {
    if (all(unlist(lapply(p, ggplot2::is.ggplot)))) {

      cli::cli_progress_bar("Saving images", total = length(p))

      if (is.null(path)) {
        path <- unlist(lapply(1:length(p), function(i) tempfile(fileext = ".png")))
      }

      if (length(path) != length(p)) stop("The number of paths does not match the number of plots provided!")

      for (i in 1:length(p)) {
        ggplot2::ggsave(path[[i]], plot = p[[i]], width = width, height = height, ...)
        cli::cli_progress_update()
      }
      cli::cli_progress_done()

      return(path)
    }
  }

  # If a single plot provided
  if (is.null(path)) {
    path <- tempfile(fileext = ".png")
  }
  ggplot2::ggsave(path, plot = p, width = width, height = height, ...)
  return(path)
}

# remove_plot -------------------------------------------------------------

#' Remove a plot
#'
#' This function removes a plot from a provided path.
#'
#' @param path Character. Path to the image.
#' @param check_ext Boolean. Whether to check the file extension.
#' @return No return. Called for side-effect.
#' @examples
#' p <- ggplot2::ggplot(cars) + ggplot2::geom_point(ggplot2::aes(dist, speed))
#' path <- save_plot(p)
#' remove_plot(path)
#'
#' @export
remove_plot <- function(path, check_ext = TRUE) {
  for(this_path in path) {
    if (!file.exists(this_path)) return(warning("File doesn't exist!"))
    if (check_ext && (!tolower(tools::file_ext(this_path)) %in% c("tif", "tiff", "bmp", "jpg", "jpeg", "gif", "png", "eps")))
      stop(paste0("File extension `", tools::file_ext(this_path), "` does not apper to be associated with an image!"))

    file.remove(this_path)
  }

  return(invisible(NULL))
}
