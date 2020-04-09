
#' \code{accordion}
#'
#' Create an accordion element for use in shiny apps. This function returns
#' a component that expands and collapses user defined content. For example,
#' you can create an FAQ page using this component, create definitions, or
#' hide anything you like. The returned component is a text element (i.e.,
#' title, question, etc.) and an html element (or \code{tagList} containing)
#' two or more html elements.
#'
#' @section Arguments
#'
#' @param title a text string containing a title for the collapsible section
#' @param html an html element or a list of html elements
#' @param ... other options to control the markup and style of the accordion
#'
#' @section Optional Arguments
#'
#' There are a number of optional arguments that are availble. These arguments
#' will allow you to control the html markup or styling of the accordion
#' element. These arguments are listed below.
#'
#' \itemize{
#'      \item \code{id}: A unique ID to be added a custom data attribute
#'              \code{data-group}. This may be useful for selecting elements
#'              in CSS. By default, a random ID is generated using a helper
#'              function as a key is needed to control the elements in
#'              javascript. Use the argument `print_id` to display the id
#'              in the R Console or terminal window.
#'      \item \code{style}: the style attributes can be used to control the
#'                  appearance of the accordion element. Arguments must be
#'                 entered as a list: \code{style = list(...)}.
#'      \itemize{
#'          \item \code{icon_background}: change the background color of the
#'              accordion toggle icon.
#'          \item \code{icon_fill}: change the color of the icon
#'      }
#'      \item \code{options}: a list of arguments used to control the
#'              structure of the accordion element. Arguments must be added as
#'              a list: \code{options = list(...)}
#'      \itemize{
#'          \item \code{heading_level}: By default, the title is rendered into
#'              a <h4> element. This element may not always work in all
#'              situations as it is difficult to determine the markup and
#'              context the accordion element is used. This option will allow
#'              you to use the accordion element with the document's hierarchy
#'              All html heading elements can be used (h1, h2, ..., or h6).
#'          \item \code{start_open}: A logical argument to determine if the
#'              accordion should be "opened" when rendered. By default, the
#'              accordion will always be closed.
#'          \item \code{print_id}: A logical argument that prints the ID used
#'              in the current instance of the accordion element. This is
#'              useful for styling the element in css. Since the returned
#'              object is a list of elements, there is no parent element to
#'              target. A unique ID is generated inside the function and added
#'              to the `data-group` attribute, which can be used as a selector
#'              in CSS. The default setting is FALSE.
#'      }
#' }
#'
#' @examples
#'
#' library(shiny)
#' library(accessibleshiny)
#' ui <- tagList(
#'      accessibleshiny::use_accessibleshiny(),
#'      accordion(
#'           title = "How do I customize my UI with HTML?",
#'           html = tagList(
#'               tags$p(
#'                   tags$code("shiny::tags"),
#'                   "is a list of 110 functions. Each function builds a",
#'                   "specific HTML tag. If you are familiar with HTML, you",
#'                   "will recognize these tags by their names."
#'               ),
#'               tags$p(
#'                   "See the"
#'                   tags$a(
#'                       href = "https://shiny.rstudio.com/articles/html-tags.html"
#'                       "shiny html tags"
#'                   ),
#'                   "guide for more information"
#'               )
#'           ),
#'           options = list(
#'               start_open = TRUE
#'           )
#'      )
#' )
#'
#' server <- function(input, output) {}
#' shinyApp(ui, server)
#'
#' @keywords accordion
#' @return Create a collapsible section
#' @export
accordion <- function(title, html, ...) {
    stopifnot(!is.null(title))
    stopifnot(!is.null(html))

    # validate props
    props <- accordion_helpers$validate_props(...)

    # build child elements
    el <- tagList(
        accordion_helpers$heading(title, props = props),
        accordion_helpers$content(html, prop = props)
    )

    # print id?
    if (props$options$print_id) {
        accordion_helpers$print_id(title, props$id)
    }

    # return
    return(el)
}