#' The datatable function creates an accessible datatable.
#'
#' @param data A data object used to render the table (required)
#' @param id A unique id for the table (required)
#' @param css A character string of css name(s) to render in the table element
#' @param caption  A short description (1-2 lines) for the table (optional)
#' @param options A list of options to pass on to the render table method
#'      @param responsive A logical arg for turning on/off the rendering of
#'             additional elements for a responsive tables (default = FALSE)
#'      @param rowHeaders a logical argument when true sets the first cell
#'             of every row as a header element (default = FALSE).
#'      @asHTML A logical argument when true will render cell values as
#'              html elements (default = FALSE)
#'
#' @section Additional Options
#' Additional options are available for customizing the output. All options must
#' be passed through as a list. See examples for implementation. The available
#' options are described below.
#'
#' \code{css}: a character string containing one or more css classes. Classes
#'          must be seperated by a space, e.g., "myclass-a myclass-b"
#'
#' @examples
#'
#' datatable(data = iris, id = "iris-table")
#'
#' datatable(data = iris, id = "iris-table", css = "dark-theme")
#' 
#' datatable(id = "iris", data = iris, options = list(responsive = T))

#' @keywords datatable a11y
#' @author David Ruvolo


# TABLE
# define a function that returns the table
datatable <- function(data, id = NULL, caption = NULL, css = NULL,
    options = list(responsive = TRUE, rowHeaders = TRUE, asHTML = FALSE)) {

    # render table and table elements
    tbl <- htmltools::tags$table(class = "datatable",
        datatable_helpers$build_header(data, options),
        datatable_helpers$build_body(data, options)
    )

    # add id
    if (length(id) > 0) {
        tbl$attribs$id <- id
    }

    # add css
    if (length(css) > 0) {
        tbl$attribs$class <- css
    }

    # should a caption be rendered?
    if (!is.null(caption)) {
        tbl$children <- list(
            htmltools::tags$caption(caption),
            tbl$children
        )
    }

    # return
    tbl
}