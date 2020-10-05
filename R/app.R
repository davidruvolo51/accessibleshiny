#' Accessible Shiny App
#'
#' Primary function for initiating the accessible shiny package client side.
#' This function is required in order to use this package. You can also use
#' this function to document attributes, such as language.
#'
#' @param title a title for the document
#' @param lang specify the language of the document
#' @param direction specify the direction in which the document should be read
#'          Use "rtl" (right to left), "ltr" (left to right), or "auto"
#' @param classnames css classes to apply to the parent container
#' @param ... the content of the shiny app
#'
#' @examples
#' if (interactive()) {
#'   ui <- accessibleshiny::app(
#'     title = "My Shiny App",
#'     lang = "en",
#'     direction = "ltr",
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
#' @importFrom htmltools tagList tags
#' @return load package assets into the app and define the document attributes
#' @export
app <- function(
    title = "",
    lang = "en",
    direction = "ltr",
    classnames = NULL,
    ...
    ) {

    # validate args
    if (title == "") {
        cli::cli_alert_warning("{.fun app}: {.arg title} is blank")
    }

    .dirs <- c("ltr", "rtl", "auto")
    if (!direction %in% .dirs) {
        cli::cli_alert_warning(
            "{.fun app}: {.val {direction}} is invalid. Use {.value {.dirs}}"
        )
    }

    b <- tags$div(class = "accessibleshiny", ...)
    if (!is.null(classnames)) {
        b$attribs$class <- paste0(b$attribs$class, " ", classnames)
    }

    tagList(
        tags$head(
            tags$meta(charset = "utf-8"),
            tags$meta(
                `http-quiv` = "x-ua-compatible",
                content = "ie=edge"
            ),
            tags$meta(
                name = "viewport",
                content = "width=device-width, initial-scale=1"
            ),
            htmltools::htmlDependency(
                version = "0.0.1",
                name = "accessibleshiny",
                src = "accessibleshiny/public/",
                package = "accessibleshiny",
                stylesheet = "accessibleshiny.min.css",
                script = "accessibleshiny.min.js",
                all_files = FALSE
            ),
            tags$title(title)
        ),
        tags$span(
            id = "accessible__shiny__meta",
            style = "display: none;",
            `data-html-lang` = lang,
            `data-html-dir` = direction
        ),
        b
    )
}