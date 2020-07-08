#'////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-07-08
#' MODIFIED: 2020-07-08
#' PURPOSE: a dev app for developing and debugging components
#' STATUS: on.going
#' PACKAGES: shiny, accessibleshiny
#' COMMENTS: This application should be used for developing and debugging
#' new components for the accessibleshiny package. Building new components
#' will require a lot of debugging as there are many elements to test.
#' Components in the accessibleshiny package usually have a primary function
#' and a series of helper functions, as well as css, javascript functions, and
#' svg elements. This app was designed to make the development process a bit
#' easier as you can write R code, apply CSS, create JavaScript input
#' bindings and functions, and integrate other assets without having to rebuild
#' the package and create a separate demo application.
#'
#' Here is what you will need to get started.
#'
#' 1. Load a component and helpers from the R/ folder
#' 2. Start the development server (which will initiate Shiny and Parcel)
#' 3. Develop the component and document
#' 4. Use accessiblity tools (I am using WAVE)
#' 5. Write unit tests (a test for every option)
#'
#'////////////////////////////////////////////////////////////////////////////

# load pkgs used by the component
suppressPackageStartupMessages({
    library(shiny)
    library(rheroicons)
})

# load files here
source("../../R/accordion.R")
source("../../R/helpers-accordion.R")


# add resource path
addResourcePath(
    "accessibleshiny",
    directoryPath = "~/Github/accessibleshiny/inst/accessibleshiny/"
)

# define ui
ui <- tagList(
    tags$head(
        tags$meta(charset = "utf-8"),
        tags$meta(
            `http-quiv` = "x-ua-compatible",
            content = "ie=edge"
        ),
        tags$meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
        ),
        tags$link(
            rel = "stylesheet",
            href = "accessibleshiny/accessibleshiny.min.css"
        ),
        tags$style(
            "html, body {
                font-family: Helvetica;
                font-size: 16pt;
            }",
            "#what-is-shiny {
                width: 400px;
            }"
        ),
        tags$title("Accessible Shiny Dev & Debugging App")
    ),
    # main document area
    tags$main(
        id = "main",
        class = "main",
        tags$h2("Accordion Component Debugging"),
        accordion(
            inputId = "what-is-shiny",
            title = "What is Shiny?",
            html = tagList(
                tags$p(
                    "Shiny is an R package that makes it easy to build",
                    "interactive web apps straight from R. You can host",
                    "standalone apps on a webpage or embed them in R Markdown",
                    "documents or build dashboards. You can also extend your",
                    "Shiny apps with CSS themes, htmlwidgets, and JavaScript",
                    "actions."
                ),
                tags$cite("Rstudio.org")
            )
        )
    ),
    # load built javascript
    tags$script(src = "accessibleshiny/accessibleshiny.min.js")
)

# define server
server <- function(input, output, session) {
}


# define app
shinyApp(ui, server)