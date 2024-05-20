check_type <- function(object, type, length = NULL) {

}

check_int_vec <- function(obj, length = NULL) {
  is.integer(obj)
}

#' Check if a library is available
#' @export
check_python_library_available <- function(lib_name) {
  find_spec <- reticulate::import("importlib.util", convert = FALSE)$find_spec
  module_spec <- find_spec(lib_name)

  if (is.null(reticulate::py_to_r(module_spec)))
    stop(paste0("Library `", lib_name, "` can not be found in the currently used version of Python! Consider using `reticulate::py_config()` to confirm the version of Python is correct and using `reticulate::use_python()` to select a Python interpreter."))
}
