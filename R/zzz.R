
# add resources
.onLoad <- function(libname, pkgname) {
    addResourcePath(
        prefix = "accessibleshiny",
        directoryPath = system.file(
            "accessibleshiny",
            package = "accessibleshiny"
        )
    )
}

# remove resources
.onUnload <- function(libname, pkgname) {
    removeResourcePath(prefix = "accessibleshiny")
}