#' Listbox
#'
#' Generate a listbox widget based on user defined options.
#'
#' @param inputId a unique ID for the input element
#' @param title a title that describes the select input component
#' @param label a label that describes what to do (optional)
#' @param options an array used to generate input options
#' @param values an array of values to pass on to each inputs (gets `options`
#'          if argument is NULL)
#' @param classnames a string containing one or more css classes to
#'          attach to the returned element
#'
#' @examples
#' listbox(
#'     inputId = "popularTech",
#'     title = "The Most Popular Technologies",
#'     label = "Select a technology",
#'     options = c(
#'         "JavaScript",
#'         "HTML/CSS",
#'         "SQL",
#'         "Python",
#'         "Java",
#'         "Bash/Shell/Powershell",
#'         "C#",
#'         "PHP",
#'         "Typescript",
#'         "C++"
#'     ),
#'     values = c(
#'         "js",
#'         "html_css",
#'         "sql",
#'         "py",
#'         "java",
#'         "bsh_sh_powershell",
#'         "csharp",
#'         "php",
#'         "typescript",
#'         "cpp"
#'     ),
#'     classnames = "my-listbox-style"
#' )
#' 
#' @return Create a listbox component
#'
#' @export
listbox <- function(
    inputId,
    title,
    label = NULL,
    options,
    values = NULL,
    classnames = NULL
) {

    # validate
    stopifnot(
        "`inputId` must be a string" = is.character(inputId),
        "`title` must be a string" = is.character(title),
        "`options` cannot blank" = !is.null(options)
    )

    # process options + values
    data <- list(options = options, values = options)
    if (!is.null(values)) data$values <- values

    # set ID of text elements (for aria attributes)
    title_id <- paste0(inputId, "__title")
    label_id <- paste0(inputId, "__label")

    # build component
    el <- tags$fieldset(
        id = inputId,
        class = "listbox",
        `data-group` = inputId,
        `data-value` = "NULL",
        tags$legend(
            id = title_id,
            class = "listbox__title",
            title
        ),
        .listbox__helpers$ui__toggle(
            inputId = inputId,
            title_id = title_id
        ),
        .listbox__helpers$ui__list(
            inputId = inputId,
            data = data,
            title_id = title_id
        )
    )

    # append label if present
    if (!is.null(label)) {
        stopifnot("'label' must be a string" = is.character(label))
        el$children <- tagList(
            el$children[1],
            tags$span(
                id = label_id,
                class = "listbox__label",
                label
            ),
            el$children[2],
            el$children[3],
        )
    }

    if (!is.null(classnames)) {
        stopifnot("'classnames' must be a string" = is.character(classnames))
        el$attribs$class <- paste0(el$attribs$class, " ", classnames)
    }

    return(el)
}


#' Reset Listbox
#'
#' Reset the listbox component to it's initial state or a specific option.
#' The initial state, is the first available option.
#'
#' @param inputId the ID of the listbox to reset
#' @param value the name of the option to select (i.e.,
#'      `listbox(choices = c(...))`) or a value (i.e.,
#'      `listbox(values = c(...))`). If `value` is left blank, then the
#'      first option is selected.
#'
#' @return Reset a listbox to the first or named option
#'
#' @export
reset_listbox <- function(inputId, value = NULL) {

    val <- ""
    if (!is.null(value)) {
        stopifnot("'option' must be a string" = is.character(value))
        val <- value
    }

    session <- shiny::getDefaultReactiveDomain()
    session$sendInputMessage(
        inputId = inputId,
        message = list(
            type = "reset",
            value = val
        )
    )
}