#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-09-09
#' MODIFIED: 2020-09-16
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
# usethis::use_package("R6")
usethis::use_package("rheroicons", min_version = TRUE)


#'//////////////////////////////////////

#' ~ 1 ~
#' Package Development and Testing

# development
devtools::check_man()
shiny::runApp(appDir = "dev/dev-app", port = 8000, launch.browser = FALSE)

# tests
devtools::test()
devtools::check()


#'//////////////////////////////////////

#' ~ 2 ~
#' Misc Checks

# birds <- read.csv("dev/dev-app/data/birds_summary.csv")

# devtools::load_all()
# datatable(data = birds[1, ], caption = "Bird Counts")



#'//////////////////////////////////////


#' ~ 99 ~
#' Package Management

# pkgbump
pkgbump::set_pkgbump(
    files = c(
        "DESCRIPTION",
        "package.json",
        "R/use_accessibleshiny.R"
    )
)

pkgbump::pkgbump(version = "0.0.1")



# ignore files
.ignore <- c(
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    ".sass-cache",
    ".cache",
    "node_modules",
    ".yarn",
    ".DS_Store"
)
usethis::use_git_ignore(.ignore)
usethis::use_build_ignore(
    files = c(
        .ignore,
        "dev",
        "inst/src",
        "accessibleshiny.code-workspace",
        ".postcssrc",
        ".babelrc",
        "package.json",
        "yarn.lock",
        ".pkgbump.json"
    )
)