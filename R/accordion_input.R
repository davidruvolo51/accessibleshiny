
#' \code{accordion_input}
#'
#' Create an accordion input element for use in shiny apps. This function
#' function a component that expands and collapses user defined content. The
#' component is designed to be selectable i.e., accessible in the shiny
#' server via `input$*`. If you would display content that is collapsible,
#' use the `accordion` component.
#'
#' This component is useful for creating checkbox groups that have further
#' descriptions or other information than may be useful for the user in regards
#' to which element should be selection.
#'
#' @param inputId a unique ID for the accordion component
#' @param title a text string containing a title for the collapsible section
#' @param content an html element or a list of html elements
#' @param checked a logical argument that indicates if the item should be
#'      selected by default (default: FALSE)
#' @param class a string containing css classes. Using this argument, you can
#'      pass your own class names or use one of the classes made available by
#'      this package: "accordion__style__a" (styling used in the app). Use
#'      `class = NULL`, to return a minimally styled component.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- tagList(
#'     iceComponents::use_iceComponents(),
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
#'       tags$h2("Is Shiny your favorite R technology?"),
#'       iceComponents::accordion_input(
#'         inputId = "what-is-shiny",
#'         title = "Shiny",
#'         checked = TRUE,
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
#'       tags$button(
#'         id = "reset",
#'         class = "shiny-bound-input action-button",
#'         "Reset Accordion"
#'       ),
#'       tags$button(
#'         id = "clear",
#'         class = "shiny-bound-input action-button",
#'         "Clear Accordion"
#'       )
#'     )
#'   )
#'   server <- function(input, output) {
#'     observeEvent(input$reset, {
#'       iceComponents::reset_accordion_input(inputId = "what-is-shiny")
#'     })
#'     observeEvent(input$clear, {
#'       iceComponents::clear_accordion_input(inputId = "what-is-shiny")
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @importFrom htmltools tags tagList
#' @return Create an accordion component that is also a checkbox input
#' @export
accordion_input <- function(
    inputId,
    title,
    content,
    checked = FALSE,
    classnames = NULL,
    style = "flat"
) {

    # validate
    if (!is.character(inputId)) stop("argument 'inputId' must be a string")
    if (!is.character(title)) stop("argument 'title' must be a string")
    if (is.null(content)) stop("argument 'content' cannot be null")
    if (!is.logical(checked)) stop("argument 'checked' must be TRUE or FALSE")

    # define ids
    ids <- accordion_input_helpers$set_html_ids(inputId = inputId)

    # build child elements
    el <- tags$div(
        id = ids$group,
        class = paste0("accordion__input accordion__", style),
        `data-accordion-initial-state` = tolower(checked),
        accordion_input_helpers$heading(
            ids = ids,
            title = title,
            checked = checked
        ),
        accordion_input_helpers$content(
            ids = ids,
            content = content
        )
    )

    # append class (if applicable)
    if (!is.null(classnames)) {
        el$attribs$class <- paste0(
            el$attrib$class, " ", classnames
        )
    }

    # return
    return(el)
}


#' \code{reset_accordion_input}
#'
#' A server-side function that resets the accordion input component to it's
#' default statue. The collapsible will be closed and the input will be
#' checked depending on the default settting.
#'
#' @param inputId the ID of the component to reset
#'
#' @export
reset_accordion_input <- function(inputId) {
    session <- shiny::getDefaultReactiveDomain()
    session$sendInputMessage(
        inputId = inputId,
        message = "resetAccordionInput"
    )
}

#' \code{clear_accordion_input}
#'
#' A server-side function that clears the accordion input component
#' regardless of the default setting. Running this function will collapse
#' the accordion and uncheck the input.
#'
#' @param inputId the ID of the component to clear
#'
#' @export
clear_accordion_input <- function(inputId) {
    session <- shiny::getDefaultReactiveDomain()
    session$sendInputMessage(
        inputId = inputId,
        message = "clearAccordionInput"
    )
}