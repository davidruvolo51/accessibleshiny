#' Accordion Helpers
#'
#' Create a series of secondary functions that generate child elements of the
#' accordion and process arguments. These functions are added to a nested list
#' object that is called inthe main function. Functions that are used to
#' generate UI elements should be placed in .accordion__helpers$ui$...
#'
#' @noRd
.accordion__helpers <- list()


#' Set Namespace IDs for component
#'
#' Define a function that generates the IDs and data attributes for properly
#' linking elements across helper functions. This function should be run
#' in the parent `accordion` function, and then passed down to individual
#' helper functions.
#'
#' @param inputId a user defined
#'
#' @noRd
.accordion__helpers$ui__ids <- function(inputId) {
    ns <- shiny::NS(namespace = inputId)
    ids <- list(
        group = inputId,
        heading_id = ns("accordion-heading"),
        button_id = ns("accordion-btn"),
        content_id = ns("accordion-section")
    )
    return(ids)
}

#' accordion heading
#'
#' Define a function that returns the heading of the accordion section. This
#' function returns a heading element (`h4`) that places the title in a button.
#' The button is used to open and close the hidden section. This function is
#' used in the primary function along with the `content()` function.
#'
#' @param ids a list object generated by set_html_ids
#' @param title user defined title
#' @param props the processed arguments generated from the validate_props func
#'
#' @importFrom htmltools tags tagList
#'
#' @noRd
.accordion__helpers$ui__heading <- function(ids, title, heading_level) {
    tags[[heading_level]](
        id = ids$heading_id,
        class = "accordion__heading",
        `data-accordion-group` = ids$group,
        # <button>
        tags$button(
            id = ids$button_id,
            class = "accordion__toggle",
            `data-accordion-group` = ids$group,
            `aria-controls` = ids$content_id,
            `aria-expanded` = "false",
            tags$span(
                class = "toggle__label",
                title
            ),
            rheroicons::rheroicon(
                name = "chevron_down",
                type = "outline",
                classnames = "toggle__icon"
            )
        )
    )
}

#' accordion content
#'
#' Define a function that creates the collapsible element in the accordion.
#' This function is used in the primary function along with the `heading()`
#' function.
#'
#' @param ids a list object generated by set_html_ids
#' @param content user defined html element or tagList of elements
#'
#' @noRd
.accordion__helpers$ui__content <- function(ids, content) {
    tags$section(
        id = ids$content_id,
        class = "accordion__content",
        `aria-labelledby` = ids$heading_id,
        `data-accordion-group` = ids$group,
        hidden = "",
        content
    )
}