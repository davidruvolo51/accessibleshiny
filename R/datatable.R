#' Accessible and Responsive Datatables
#'
#' The `datatable` function creates an accessible, responsive table from a
#' dataset. The function returns a shiny tagList object which can
#' be used in shiny applications or markdown documents. This function
#' can also be used as an html table generator and the output can be
#' written to file. This function takes the following arguments.
#'
#'
#' @param data A data object used to render the table (required)
#' @param caption A short description (1-2 lines) for the table (optional)
#' @param caption_placement change the position of the caption in relation to
#'      the table. Choices are "top" (default) or "bottom".
#' @param id a unique ID for the table
#' @param classnames a string containing one or more css classes
#' @param row_highlighting If `TRUE` (default), whenever the mouse hovers over
#'      a cell, the entire row will be highlighted
#' @param row_headers If `TRUE`, the first cell in each table row will be
#'      rendered as a row header (default: `FALSE`).
#' @param is_responsive If `TRUE` (default), the HTML structure of the table
#'      will be responsive.
#' @param html_escape If `TRUE` (default), all cell content will be rendered
#'      as plain text.
#'
#' @examples
#' 
#' ```{r}
#' datatable(data = iris)
#'
#' datatable(data = iris, id = "iris-table", classnames = "dark-theme")
#'
#' datatable(data = iris, is_responsive = FALSE)
#'
#' datatable(data = iris, id = "iris", row_headers = TRUE)
#'
#' df <- dplyr::starwars
#' df$link <- paste0(
#'     "<a href='https://www.google.com/search?q=",
#'     gsub(" ", "+", df$character),
#'     "'>",
#'     df$character,
#'     "</a>"
#' )
#' tbl <- datatable(data = df, html_escape = FALSE)
#' writeLines(as.character(tbl), "~/Desktop/table.html")
#' ```
#'
#' @return Create a responsive datatable
#'
#' @export
datatable <- function(
    data,
    caption = NULL,
    caption_placement = NULL,
    id = NULL,
    classnames = NULL,
    row_highlighting = TRUE,
    row_headers = FALSE,
    is_responsive = TRUE,
    html_escape = TRUE
) {

    # validate input args
    stopifnot(
        "`row_highlighting` must be TRUE or FALSE" = is.logical(row_highlighting),
        "`row_headers` must be TRUE or FALSE" = is.logical(row_headers),
        "`is_responsive` must be TRUE or FALSE" = is.logical(is_responsive),
        "`html_escape` must be TRUE or FALSE" = is.logical(html_escape)
    )

    if (!is.null(caption_placement)) {
        stopifnot(
            "`caption_placement` must be 'top' or 'bottom'" = {
                caption_placement %in% c("top", "bottom")
            }
        )
    }

    # build value for class attribute
    css <- .datatable__helpers$validate__classnames(
        caption_status = ifelse(is.null(caption), FALSE, TRUE),
        caption_placement = ifelse(
            is.null(caption_placement),
            "top",
            caption_placement
            ),
        row_highlighting = row_highlighting,
        is_responsive = is_responsive
    )

    # gather options
    config <- list(
        is_responsive = is_responsive,
        html_escape = html_escape,
        row_headers = row_headers
    )

    # generate table markup
    tbl <- tags$table(
        class = css,
        .datatable__helpers$ui__thead(data, config = config),
        .datatable__helpers$ui__tbody(data, config = config)
    )

    # append caption
    if (length(caption) > 0) {
        tbl$children <- list(
            tags$caption(
                as.character(caption)
            ),
            tbl$children
        )
    }

    # add `id` and `class`
    if (!is.null(id)) {
        tbl$attribs$id <- id
    }
    if (!is.null(classnames)) {
        tbl$attribs$class <- paste0(tbl$attribs$class, " ", classnames)
    }

    # return
    return(tbl)
}