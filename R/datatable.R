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
#' @parma ... Other list object used to control the component. See the next
#'      section and the wiki for more information.
#'
#' @section Optional Arguments
#' Using \code{...}, you can pass additional arguments that allow you to
#' control the markup, css attributes, etc. These arguments must be passed
#' as list objects through \code{style = list()} or by using
#' \code{options = list()}. These arguments are explained below. See the wiki
#' for more information.
#' \code{style} a list of options to control the appearance of the app
#'      \code{enabled} When TRUE (default), all css classes will be applied.
#'              FALSE will return a table with no css classes. This will also
#'              overide the argument \code{loadDependency}, \code{responsive},
#'      \code{rowHighlighting} a logical value when true will highlight odd rows
#' \code{options} A list of options to pass on to the render table method
#'      \code{responsive} A logical arg for turning on/off the rendering of
#'             additional elements for a responsive tables (default = FALSE)
#'      \code{rowHeaders} a logical argument when true sets the first cell
#'             of every row as a header element (default = FALSE).
#'      \code{asHTML} A logical argument when true will render cell values as
#'              html elements (default = FALSE)
#'      \code{loadDependency} When TRUE (default), the function will load the
#'              corresponding css files into the header document. FALSE will
#'              not load datatable dependencies.
#' 
#' @examples
#' datatable(data = iris, id = "iris-table")
#'
#' datatable(data = iris, id = "iris-table", css = "dark-theme")
#'
#' datatable(id = "iris", data = iris, options = list(responsive = TRUE))
#'
#' datatable(id = "iris", data = iris, options = list(asHTML = TRUE))
#'
#' df <- iris
#' df$link <- paste0(
#'     "<a href='https://www.google.com/search?q=",
#'     gsub(" ", "+", df$link),
#'     "'>",
#'     df$link,
#'     "</a>"
#' )
#' tbl <- datatable(id = "iris", data = iris, options = list(asHTML = TRUE))
#' writeLines(as.character(tbl), "~/Desktop/iris_table.html")
#' @return Returns an html object, i.e., shiny tagList. Use
#'         \code{options = list(...)} for addtional rendering options.
#' @keywords datatable a11y
#' @author dcruvolo
#' @importFrom htmltools tags
#'
datatable <- function(data, id = NULL, caption = NULL, css = NULL, ...) {

    # validate args: send `...` down to validation function. The returned
    # output is nested lists. To access optiions and props, use `props$style`
    # or `props$options`.
    props <- datatable_helpers$validate_props(...)

    # render table and table elements
    tbl <- htmltools::tags$table(
        datatable_helpers$build_header(
            data = data,
            options = props$options
        ),
        datatable_helpers$build_body(
            data = data,
            style = props$style,
            options = props$options
        )
    )

    # update table attributes - apply css only if style$enabled == TRUE
    tbl$attribs <- datatable_helpers$set_table_attributes(
        id = id,
        css = css,
        style = props$style
    )

    # should a caption be rendered?
    if (length(caption) > 0) {
        tbl$children <- list(
            htmltools::tags$caption(caption),
            tbl$children
        )
    }

    # load css from ww/css/datatables.css if applicable
    if (isFALSE(props$style$enabled) | isFALSE(props$options$loadDependency)) {
        tbl
    } else {
        htmltools::tagList(
            datatable_helpers$load_dependencies(),
            tbl
        )
    }
}