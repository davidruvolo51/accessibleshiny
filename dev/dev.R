#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-09-09
#' MODIFIED: 2020-09-09
#' PURPOSE: pkg management
#' STATUS: ongoing
#' PACKAGES: usethis; pkgbump
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

# pkgs
remotes::install_github("davidruvolo51/pkgbump")
remotes::install_github("davidruvolo51/rheroicons")


# init pkg
usethis::use_news_md()
usethis::use_description()
usethis::use_namespace()
#' usethis::use_github_action_check_standard()
#' usethis::use_travis()


# set pkg dependencies
usethis::use_package("shiny")
usethis::use_package("htmltools")
usethis::use_package("purrr")
usethis::use_package("rlang")
usethis::use_package("R6")
usethis::use_package("cli")
usethis::use_package("rheroicons", min_version = TRUE)


# development
devtools::check_man()
shiny::runApp(appDir = "dev/dev-app", port = 8000, launch.browser = FALSE)

# tests
devtools::check()

# pkgbump
pkgbump::set_pkgbump(
    files = c(
        "DESCRIPTION",
        "package.json",
        "R/use_accessibleshiny.R"
    )
)

pkgbump::pkgbump(version = "0.0.1")