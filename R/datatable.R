#' Accessible and Responsive Datatables
#'
#' The \code{datatable} function creates an accessible, responsive table from a
#' dataset. The function returns a shiny tagList object which can
#' be used in shiny applications or markdown documents. This function
#' can also be used as an html table generator and the output can be
#' written to file. This function takes the following arguments.
#'
#' @section Arugments
#'
#' @param data A data object used to render the table (required)
#' @param caption  A short description (1-2 lines) for the table (optional)
#' @param ... Other list object used to control the component. See the next
#'      section and the wiki for more information.
#'
#' Using \code{...}, you can pass additional arguments that allow you to
#' control the markup, css attributes, etc. The available options are
#' \code{id}, \code{class}, \code{style}, and \code{options}. The later two
#' must be entered as a list object.
#' \code{id} a unique id to pass to the \code{<table>} markup
#' \code{class} a string containing one or more css classnames to add to the
#'      \code{<table class="">} attribute.
#' \code{style} a list of options to control the appearance of the app
#'      \code{row_highlighting} a logical value when true will highlight
#'      odd rows
#' \code{options} A list of options to pass on to the render table method
#'      \code{responsive} A logical arg for turning on/off the rendering of
#'             additional elements for a responsive tables (default = TRUE)
#'      \code{row_headers} a logical argument when true sets the first cell
#'             of every row as a header element (default = FALSE).
#'      \code{html_escape} A logical argument when true will render cell values
#'              as plain text or as html elements (default = TRUE; i.e., escaped)
#'
#' @examples
#' datatable(data = iris)
#'
#' datatable(data = iris, id = "iris-table", class = "dark-theme")
#'
#' datatable(data = iris, options = list(responsive = FALSE))
#'
#' datatable(id = "iris", data = iris, options = list(row_headers = TRUE))
#'
#' df <- iris
#' df$link <- paste0(
#'     "<a href='https://www.google.com/search?q=",
#'     gsub(" ", "+", df$link),
#'     "'>",
#'     df$link,
#'     "</a>"
#' )
#' tbl <- datatable(data = iris, options = list(html_escape = FALSE))
#' writeLines(as.character(tbl), "~/Desktop/iris_table.html")
#' @return Returns an html object, i.e., shiny tagList. Use
#'         \code{options = list(...)} for addtional rendering options.
#' @keywords datatable a11y
#' @author dcruvolo
datatable <- function(data, caption = NULL, ...) {

    # validate input args
    props <- validate_props(...)

    # generate table markup
    tbl <- tags$table(
        thead(data, options = props$options),
        tbody(data, options = props$options)
    )

    # update table attributes
    tbl$attribs <- props$attribs

    # append caption
    if (length(caption) > 0) {
        tbl$children <- list(
            tags$caption(caption),
            tbl$children
        )
    }

    # return
    return(tbl)
}