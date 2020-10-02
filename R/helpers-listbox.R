#' Listbox Helper functions
#'
#' Nest listbox functions into a single object
#'
#' @noRd
.listbox__helpers <- list()


#' Listbox toggle
#'
#' Render listbox button that displays the selected option and that has an
#' icon that indicates the button shows/hides an expandable list.
#'
#' @param inputId the inputId passed down from the parent function
#' @param title_id ID of the component's title
#'
#' @noRd
.listbox__helpers$ui__toggle <- function(inputId, title_id) {
    btn_id <- paste0(inputId, "__", "toggle")
    btn_label_id <- paste0(btn_id, "_label")

    htmltools::tags$button(
        id = btn_id,
        class = "listbox__toggle",
        `data-group` = inputId,
        `aria-haspopup` = "listbox",
        `aria-expanded` = "false",
        `aria-labelledby` = paste0(title_id, " ", btn_label_id),

        # text element for current selected item
        htmltools::tags$span(
            id = btn_label_id,
            class = "toggle__text"
        ),

        # icon
        rheroicons::rheroicon(
            name = "chevron_down",
            type = "solid",
            classnames = "toggle__icon"
        )
    )
}


#' List Options Item
#'
#' This function will render a list item which will have a
#' an svg icon and a text label.
#'
#' @param inputId the inputId passed down from the parent function
#' @param option a title for the input item
#' @param value a value for the input item (if null, will be `option`)
#'
#' @noRd
.listbox__helpers$ui__list__item <- function(inputId, option, value) {
    for_id <- paste0(inputId, "__", option)

    # generate html
    htmltools::tags$li(
        id = for_id,
        class = "listbox__option",
        role = "option",
        `data-value` = value,
        `data-group` = inputId,
        `data-option` = option,
        `aria-labelledby` = for_id,

        # selected icon
        rheroicons::rheroicon(
            name = "check_circle",
            type = "solid",
            classnames = "option__icon"
        ),
        # label
        htmltools::tags$span(
            id = paste0(for_id, "-input-label"),
            class = "option__text",
            option
        )
    )
}


#' List Options
#'
#' This function will generate the list items based on the number of options
#' indicated in the parent function.
#'
#' @param inputId the inputId passed down from the parent function
#' @param title_id Id of the component (generated internally)
#' @param data a list object containing the options and values.
#'
#' @noRd
.listbox__helpers$ui__list <- function(inputId, title_id, data) {

    # generate markup for parent element
    parent <- htmltools::tags$ul(
        id = paste0(inputId, "__input_list"),
        class = "listbox__list hidden",
        `data-group` = inputId,
        `aria-labelledby` = title_id,
        role = "listbox",
        tabindex = "-1"
    )

    # generate markup for child (<li>) elements
    children <- lapply(seq_len(length(data$options)), function(i) {
        .listbox__helpers$ui__list__item(
            inputId = inputId,
            option = data$options[[i]],
            value = data$values[[i]]
        )
    })

    # bind to parent (you can add blank option here)
    parent$children <- children

    # return
    return(parent)
}