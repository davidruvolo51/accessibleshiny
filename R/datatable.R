#' Accessible and Responsive Datatables
#'
#' The \code{datatable} function creates an accessible, responsive table from a
#' dataset. The function returns a shiny tagList object which can
#' be used in shiny applications or markdown documents. This function
#' can also be used as an html table generator and the output can be
#' written to file. This function takes the following arguments.
#'
#' @section Arugments
#' @param data A data object used to render the table (required)
#' @param id A unique id for the table (required)
#' @param css A character string of css name(s) to render in the table element
#' @param caption  A short description (1-2 lines) for the table (optional)
#' @param style a list of options to control the appearance of the app
#'      @param rowHighlighting a logical value when true will highlight odd rows
#' @param options A list of options to pass on to the render table method
#'      @param responsive A logical arg for turning on/off the rendering of
#'             additional elements for a responsive tables (default = FALSE)
#'      @param rowHeaders a logical argument when true sets the first cell
#'             of every row as a header element (default = FALSE).
#'      @asHTML A logical argument when true will render cell values as
#'              html elements (default = FALSE)
#'
#' @examples
#' datatable(data = iris, id = "iris-table")
#'
#' datatable(data = iris, id = "iris-table", css = "dark-theme")
#'
#' datatable(id = "iris", data = iris, options = list(responsive = T))
#'
#' datatable(id = "iris", data = iris, options = list(asHTML = T))
#'
#' tbl <- datatable(id = "iris", data = iris, options = list(asHTML = T))
#' writeLines(as.character(tbl), "~/Desktop/iris_table.html")
#' @return Returns an html object, i.e., shiny tagList. Use
#'         \code{options = list(...)} for addtional rendering options.
#' @keywords datatable a11y
#' @author dcruvolo
#' @importFrom htmltools singleton htmlDependencies tags
#'
datatable <- function(data, id = NULL, caption = NULL, css = NULL,
style = list(rowHighlighting = TRUE),
options = list(responsive = TRUE, rowHeaders = TRUE, asHTML = FALSE)) {

    # render table and table elements
    tbl <- htmltools::tags$table(
        class = datatable_helpers$datatable_css(css = css, style = style),
        datatable_helpers$build_header(data, options),
        datatable_helpers$build_body(data, options)
    )

    # add id
    if (length(id) > 0) {
        tbl$attribs$id <- id
    }

    # should a caption be rendered?
    if (length(caption) > 0) {
        tbl$children <- list(
            htmltools::tags$caption(caption),
            tbl$children
        )
    }

    # load css from ww/css/datatables.css and return tbl
    htmltools::tagList(
        datatable_helpers$datatable_dependencies(),
        tbl
    )
}