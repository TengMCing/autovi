# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiate methods
#' @description These functions are the same as `class$instantiate`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default. Default is `init_call = sys.call()`.
NULL

#' @describeIn portal [AUTO_VI]
#' @export
auto_vi <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  AUTO_VI$instantiate(..., env = env, init_call = init_call)
}


.onLoad <- function(libname, pkgname) {

  # Classes are empty environments defined by new.env()
  # Build them at load-time to ensure dependencies (e.g. bandicoot::BASE) are latest

  class_AUTO_VI(AUTO_VI)
}
