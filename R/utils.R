# check_python_library_available ------------------------------------------

#' Check if a library is available
#' @export
check_python_library_available <- function(lib_name) {
  find_spec <- reticulate::import("importlib.util", convert = FALSE)$find_spec
  module_spec <- find_spec(lib_name)

  if (is.null(reticulate::py_to_r(module_spec)))
    stop(paste0("Library `", lib_name, "` can not be found in the currently used version of Python! Consider using `reticulate::py_config()` to confirm the version of Python is correct and using `reticulate::use_python()` to select a Python interpreter."))
}


# save_plot ---------------------------------------------------------------

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
      path <- list()
      for (i in 1:length(p)) {
        path[[i]] <- tempfile(fileext = ".png")
        ggplot2::ggsave(path[[i]], plot = p[[i]], width = width, height = height, ...)
        cli::cli_progress_update()
      }
      cli::cli_progress_done()

      return(path)
    }
  }

  # If a single plot provided
  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(path, plot = p, width = width, height = height, ...)
  return(path)
}

# remove_plot -------------------------------------------------------------

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
