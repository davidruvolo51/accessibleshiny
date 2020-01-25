
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

# remove resources
.onUnload <- function(libname, pkgname) {
    shiny::removeResourcePath(
        prefix = "assets"
    )
}