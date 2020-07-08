
#' \code{accordion}
#'
#' Create an accordion element for use in shiny apps. This function returns
#' a component that expands and collapses user defined content. For example,
#' you can create an FAQ page using this component, create definitions, or
#' hide anything you like. The returned component is a text element (i.e.,
#' title, question, etc.) and an html element (or \code{tagList} containing)
#' two or more html elements.
#'
#' @param inputId a unique ID for the accordion component
#' @param title a text string containing a title for the collapsible section
#' @param html an html element or a list of html elements
#' @param heading_level adjust the HTML heading level; default is "h3". Use
#'      on of the following headings: h1, h2, h3, h4, h5, h6
#'
#' **Notes on `heading_level`** By default, the title is rendered into
#' a <h3> element. This element may not always work in all
#' situations as it is difficult to determine the markup and
#' context the accordion element is used. This option will allow
#' you to use the accordion element with the document's hierarchy
#' All html heading elements can be used (h1, h2, ..., or h6).
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(accessibleshiny)
#'   ui <- tagList(
#'     accessibleshiny::use_accessibleshiny(),
#'     tags$head(
#'       tags$style(
#'         "#what-is-shiny {
#'            width: 400px;
#'         }"
#'       )
#'     ),
#'     tags$main(
#'       id = "main",
#'       class = "main",
#'       tags$h2("Frequently Asked Questions Page"),
#'       tags$p("Here are some commonly asked questions")
#'       accordion(
#'         inputId = "what-is-shiny",
#'         title = "What is Shiny?",
#'         html = tagList(
#'           tags$p(
#'             "Shiny is an R package that makes it easy to build",
#'             "interactive web apps straight from R. You can host",
#'             "standalone apps on a webpage or embed them in R Markdown",
#'             "documents or build dashboards. You can also extend your",
#'             "Shiny apps with CSS themes, htmlwidgets, and JavaScript",
#'             "actions."
#'           ),
#'           tags$cite("Rstudio.org")
#'         )
#'       )
#'     )
#'   )
#'   server <- function(input, output) {}
#'   shinyApp(ui, server)
#' }
#' @keywords accessibleshiny accordion collapsible content
#' @return Create an accordion component
#' @export
accordion <- function(inputId, title, html, heading_level = "h3") {

    # validate
    if (!is.character(inputId)) stop("argument 'inputId' must be a string")
    if (!is.character(title)) stop("argument 'title' must be a string")
    if (is.null(html)) stop("argument 'html' cannot be null")

    # validate html headings
    valid_html_headings <- c("h1", "h2", "h3", "h4", "h5", "h6")
    if (!heading_level %in% valid_html_headings) {
        stop(
            paste0(
                "input for 'heading_level' is invalid. ",
                "Select: h1, h2, h3, h4, h5, or h6"
            )
        )
    }

    # define ids
    ids <- accordion_helpers$set_html_ids(inputId = inputId)

    # build child elements
    el <- tags$div(
        id = ids$group,
        class = "accordion",
        accordion_helpers$heading(
            ids = ids,
            title = title,
            heading_level = heading_level
        ),
        accordion_helpers$content(
            ids = ids,
            html = html
        )
    )

    # return
    return(el)
}
