
#' onLoad
#'
#' Load resources on package load
#'
#' @param libname required argument
#' @param pkgname required argument
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
    shiny::addResourcePath(
        prefix = "accessibleshiny",
        directoryPath = system.file(
            "accessibleshiny",
            package = "accessibleshiny"
        )
    )
}

#' onUnload
#'
#' remove resources
#'
#' @param libname required argument
#' @param pkgname required argument
#'
#' @noRd
.onUnload <- function(libname, pkgname) {
    shiny::removeResourcePath(prefix = "accessibleshiny")
}