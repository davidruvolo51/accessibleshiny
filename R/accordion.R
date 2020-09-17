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
#' @param content an html element or a list of html elements
#' @param heading_level adjust the HTML heading level; default is "h3". Use
#'      on of the following headings: h1, h2, h3, h4, h5, h6
#' @param classnames a string containing css classes. Using this argument,
#'      you can pass your own class names
#' @param style a theme for the accordion component. Use either `flat`
#'      (default) or `focused` (more hover and focus states)
#'
#' **Notes on `heading_level`** By default, the title is rendered into
#' a `h3` element. This element may not always work in all
#' situations as it is difficult to determine the markup and
#' context the accordion element is used. This option will allow
#' you to use the accordion element with the document's hierarchy
#' All html heading elements can be used (h1, h2, or h6).
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
#'         content = tagList(
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
#'       ),
#'      tags$button(
#'        id = "reset",
#'        class = "shiny-bound-input action-button",
#'        "Reset Accordion"
#'      )
#'     )
#'   )
#'   server <- function(input, output) {
#'      observeEvent(input$reset, reset_accordion("what-is-shiny"))
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @importFrom htmltools tags tagList
#' @return Create an accordion component
#' @export
accordion <- function(
    inputId,
    title,
    content,
    heading_level = "h3",
    classnames = NULL,
    style = "flat"
) {

    # validate html headings
    stopifnot(
        "input for 'heading_level' is invalid. Use: h1 through h6" = {
            heading_level %in% c("h1", "h2", "h3", "h4", "h5", "h6")
        }
    )
    stopifnot(
        "input for `style` is invalid. Use `flat` or `focused`" = {
            style %in% c("flat", "focused")
        }
    )

    # define ids
    ids <- .accordion__helpers$ui__ids(inputId = inputId)

    # build child elements
    el <- tags$div(
        id = ids$group,
        class = paste0("accordion ", "accordion__", style),
        .accordion__helpers$ui__heading(
            ids = ids,
            title = title,
            heading_level = heading_level
        ),
        .accordion__helpers$ui__content(
            ids = ids,
            content = content
        )
    )

    # append classnames (if applicable)
    if (!is.null(classnames)) {
        el$attribs$class <- paste0(el$attribs$class, " ", classnames)
    }

    # return
    return(el)
}


#' \code{reset_accordion}
#'
#' A server-side function that resets the accordion component to it's
#' default statue (closed).
#'
#' @param inputId the of of the component to reset
#'
#' @export
reset_accordion <- function(inputId) {
    session <- shiny::getDefaultReactiveDomain()
    session$sendInputMessage(
        inputId = inputId,
        message = "reset"
    )
}