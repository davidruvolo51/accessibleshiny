
# add resources
.onLoad <- function(libname, pkgname) {
    addResourcePath(
        prefix = "assets",
        directoryPath = system.file(
            "assets",
            package = "accessibleshiny"
        )
    )
}

# remove resources
.onUnload <- function(libname, pkgname) {
    removeResourcePath(prefix = "assets")
}