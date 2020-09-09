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
#' @examples
#' if (interactive()) {
#'   ui <- tagList(
#'     accessibleshiny::use_accessibleshiny(),
#'     tags$h1("Hello, world!")
#'   )
#'
#'   server <- function(input, output, session) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @references
#' \url{https://developer.mozilla.org/en-US/docs/Web/API/Document/title}
#' \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/dir}
#' \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang}
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @return load package assets into the app and define the document attributes
#' @export
use_accessibleshiny <- function(title = "", lang = "en", direction = "ltr") {

    # validate args
    .validate__use__args(title = title, lang = lang, direction = direction)

    # init accessibleshiny dependencies and <head>
    tagList(
        tags$head(

            # set meta content
            tags$meta(charset = "utf-8"),
            tags$meta(
                `http-quiv` = "x-ua-compatible",
                content = "ie=edge"
            ),
            tags$meta(
                name = "viewport",
                content = "width=device-width, initial-scale=1"
            ),

            # pkg dependencies
            htmlDependency(
                version = "0.0.1",
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
            id = "accessible__shiny__meta",
            style = "display: none;",
            `data-html-lang` = lang,
            `data-html-dir` = direction
        )
    )
}


#' validate use_accessibleshiny input values
#'
#' Validates the input values in the function `use_accessibleshiny`.
#'  This includes the values for `title`, `lang`, and `direction`.
#'
#' @param title a string to pass on to `document.title`
#' @param lang a value passed on to `<html lang="">`
#' @param direction a value passed on to `<html dir="">`
#'
#' @noRd
.validate__use__args <- function(title, lang, direction) {

    # provide messages for `title`
    if (!is.character(title)) {
        cli::cli_alert_danger("input for `title` must be a string")
    }
    if (title == "") {
        cli::cli_alert_warning("input for `title` is missing")
    }

    # provide messages for argument `lang`
    if (!is.character(lang)) {
        cli::cli_alert_danger("input for `lang` must be a string")
    }

    # provide messages for argument `direction`
    if (!is.character(direction)) {
        cli::cli_alert_danger("input for `direction` must be a string")
    }

    if (!direction %in% c("ltr", "rtl", "auto")) {
        cli::cli_alert_danger(
            "input for `lang` must be a 'ltr', 'rtl', or 'auto'"
        )
    }
}
