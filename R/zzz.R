
# add resources
.onLoad <- function(libname, pkgname) {
    shiny::addResourcePath(
        prefix = "assets",
        directoryPath = system.file(
            "assets",
            package = "accessibleshiny"
        )
    )
}