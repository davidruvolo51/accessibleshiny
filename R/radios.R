#' @title Radio Inputs
#'
#' The function \code{radios} creates a list of radio inputs, but as buttons.
#' This component is recommended for use over dropdown menus as
#' all options are visible and can be navigated with ease. This is
#' not recommended for use with variables with many options. There is
#' not limit to the number of options that can be generated, but
#' the number should be very low (e.g., < 7-8 ).
#'
#' @section Arguments
#' @param id a string containing an unique id to be applied the
#'          parent html element (i.e., fieldset; required)
#' @param css a string containing css class(es) to apply to the
#'          parent html elemt (i.e., fieldset; optional; default \code{NULL})
#' @param inputId A string or an array containing input ids to
#'          be passed on to each radio input.
#' @param labels A string or an array containing the names of each input;
#'              this item is required.
#' @param values A string or an array of values to assign to each input
#'          element (optional, but recommended).
#' @param selected an integer indicating which input element is selected 
#'          by default (optional)
#' @param caption an optional string used to create a caption for the
#'          radio group inputs (optional, but recommended.
#'
#' @examples
#' x <- c("apples", "limes", "oranges", "lemons", "bananas")
#' y <- c("Apples", "Limes", "Oranges", "Lemons", "Bananas")
#' radios(
#'      id = "fruit",
#'      css = "dark-theme",
#'      inputId = x,
#'      labels = y,
#'      values = x,
#'      selected = c(1, 3, 5),
#'      caption = "Choose 1 or more fruits"
#' )
#'
#' @return Returns html markup for a radio inputs styled as input buttons.
#' @keywords accessibleshiny, radios, inputs
#' @author dcruvolo
#' @export
radios <- function(id = "", css = NULL, inputId = c(), labels = c(),
    values = NULL, selected = NULL, caption = NULL) {

    # build data
    d <- radio_helpers$build_radio_df(
        ids = inputId,
        labels = labels,
        values = values,
        selected = selected
    )

    # build html elements
    radios <- tags$fieldset(
        role = "radiogroup",
        class = "shiny-input-radiogroup shiny-input-container radios",
        radio_helpers$build_radio_group(
            name = id,
            data = d,
            selected = selected
        )
    )

    # render caption if necessary
    if (length(caption) > 0) {
        radios$children <- list(
            htmltools::tags$legend(caption),
            radios$children
        )
    }

    # render id and css
    if (length(id) > 0) radios$attribs$id <- id
    if (length(css) > 0) {
        radios$attribs$class <- paste(radios$attribs$class, css, sep = " ")
    }

    # return
    return(
        htmltools::tagList(
            radio_helpers$radio_dependencies(),
            radios
        )
    )
}