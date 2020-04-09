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
#' @section Optional Arguments
#'
#' There are a number of optional arguments that are availble. These arguments
#' will allow you to control the html markup or styling of the accordion
#' element. These arguments are listed below.
#'
#' \itemize{
#'      \item \code{id}: a unique id to pass to the \code{<table>} markup
#'      \item \code{class}: a string containing one or more css classnames
#'          to add to the \code{<table class="">} attribute.
#'      \item \code{style}: the style attributes can be used to control the
#'                  appearance of the element. Arguments must be
#'                 entered as a list: \code{style = list(...)}.
#'      \itemize{
#'          \item \code{row_highlighting}: a logical value to enable or
#'              disable row highighting (default is TRUE)
#'          \item \code{caption_below}: a logical argument that positions
#'              the caption below the table (i.e., after). By default, the
#'              caption is placed before the table (i.e., FALSE)
#'      }
#'      \item \code{options}: a list of arguments used to control the
#'              structure of the accordion element. Arguments must be added as
#'              a list: \code{options = list(...)}
#'      \itemize{
#'          \item \code{responsive}: A logical argument for enabling or
#'              disabling the generation of the html markup that handles
#'              the responsiveness of the datatable. Default is TRUE
#'          \item \code{row_headers}: A logical argument that renders the first
#'              cell of every row as a row header element (i.e. \code{<th>};
#'              default is FALSE)
#'          \item \code{html_escape}: A logical argument to render cell content
#'              as text or as html elements. The default (TRUE) will render
#'              content as text. Use FALSE to escape.
#'      }
#' }
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

    # validate if data.frame
    stopifnot(!is.null(data))

    # validate input args
    props <- datatable_helpers$validate_props(...)

    # generate table markup
    tbl <- tags$table(
        datatable_helpers$thead(data, options = props$options),
        datatable_helpers$tbody(data, options = props$options)
    )

    # update table attributes
    tbl$attribs <- props$attribs

    # append caption
    if (length(caption) > 0) {
        tbl$children <- list(
            tags$caption(
                as.character(caption)
            ),
            tbl$children
        )
    }

    # return
    return(tbl)
}