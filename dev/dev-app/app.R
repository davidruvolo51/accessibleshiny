#' ////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-09-09
#' MODIFIED: 2020-09-09
#' PURPOSE: shiny app for component development, debuggin, testing, etc
#' STATUS: ongoing
#' PACKAGES: shiny; accessibleshiny;
#' COMMENTS: NA
#' ////////////////////////////////////////////////////////////////////////////

# pkgs
suppressPackageStartupMessages(library(shiny))

# load all
devtools::load_all()

birds <- read.csv("data/birds_summary.csv")

# add Resource Path
shiny::addResourcePath(
    "accessibleshiny",
    "~/Github/accessibleshiny/inst/accessibleshiny/"
)


#' //////////////////////////////////////

# ui
ui <- tagList(
    tags$head(
        tags$meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
        ),
        tags$link(
            rel = "stylesheet",
            href = "accessibleshiny/accessibleshiny.min.css"
        ),
        tags$style(
            HTML(
                "html, body {
                    font-family: Helvetica, Arial, sans-serif;
                    font-size: 16pt;
                }",
                "main {
                    width: 90%;
                    margin: 0 auto;
                }",
                "@media screen and (min-width: 972px) {
                    main {
                        max-width: 972px;
                    }
                }"
            )
        ),
        tags$title("Shiny Test")
    ),
    tags$main(
        tags$h1("Test App"),

        # Responsive Datatable Component
        datatable(
            data = birds[1:10, ],
            caption = "Bird Counts",
            caption_placement = "top",
            id = "bird-data-reporting-rates",
            classnames = "example-classname",
            row_highlighting = TRUE,
            row_headers = TRUE,
            is_responsive = F,
            html_escape = TRUE
        ),

        # Accordion Component Example
        # accordion(
        #     inputId = "what-is-shiny",
        #     title = "What is Shiny?",
        #     content = tagList(
        #         tags$p(
        #             "Shiny is an R package that makes it easy to build",
        #             "interactive web apps straight from R. You can host",
        #             "standalone apps on a webpage or embed them in R Markdown",
        #             "documents or build dashboards. You can also extend your",
        #             "Shiny apps with CSS themes, htmlwidgets, and JavaScript",
        #             "actions."
        #         ),
        #         tags$cite("Rstudio.org")
        #     ),
        #     style = "focused"
        # ),
        # tags$button(
        #     id = "reset",
        #     class = "shiny-bound-input action-button",
        #     "Reset"
        # )
    ),
    tags$script(src = "accessibleshiny/accessibleshiny.min.js")
)

# server
server <- function(input, output, session) {
    observeEvent(input$reset, {
        reset_accordion("what-is-shiny")
    })
}


# app
shinyApp(ui = ui, server = server)