#' use_accessibleshiny
#'
#' Primary function for initiating the accessible shiny package client side.
#' This function is required in order to use this package. You can also use
#' this function to document attributes, such as language and Open Graph Data
#'
#' @param title a title for the document
#' @param lang specify the language that the document is written in
#' @param direction specify the direction in which the document should be read
#'          Use "rtl" (right to left), "ltr" (left to right), or "auto"
#'
#' @references
#' \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/dir}
#' \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang}
#'
#' @importFrom htmltools tagList tags
#' @return Primary package function for loading assets and defining the
#'  document
#' @examples
#' use_accessiblyshiny()
#' @export
use_accessibleshiny <- function(title = "", lang = "en", direction = "ltr") {

    # validate args
    if (!is.character(lang)) stop("argument 'lang' must be a string")
    if (!is.character(title)) stop("arugment 'title' must be a string")
    if (title == "") warning("argument 'title' is blank")
    if (!is.character(direction)) stop("argument 'direction' must be a string")
    if (!direction %in% c("ltr", "rtl", "auto")) {
        stop("argument 'direction' must be 'ltr', 'rtl', or 'auto'")
    }

    # init accessibleshiny dependencies and <head>
    tagList(
        tags$head(

            # set meta content
            tags$meta(charset = "utf-8"),
            tags$meta(`http-quiv` = "x-ua-compatible", content = "ie=edge"),
            tags$meta(
                name = "viewport",
                content = "width=device-width, initial-scale=1"
            ),

            # pkg dependencies
            htmlDependency(
                version = "0.1.51",
                name = "accessibleshiny",
                src = "accessibleshiny/",
                package = "accessibleshiny",
                stylesheet = "accessibleshiny.min.css",
                script = "accessibleshiny.min.js",
                all_files = FALSE
            ),

            # document title
            tags$title(title)
        ),

        # hidden element (required for js)
        tags$span(
            id = "accessible-shiny-meta",
            style = "display: none;",
            `data-html-lang` = lang,
            `data-html-dir` = direction
        )
    )
}