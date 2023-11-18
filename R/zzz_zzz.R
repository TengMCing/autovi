.onLoad <- function(libname, pkgname) {

  # Classes are empty environments defined by new.env()
  # Build them at load-time to ensure dependencies (e.g. bandicoot::BASE) are latest

  class_AUTO_VI(AUTO_VI)
}
