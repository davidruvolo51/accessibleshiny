#' Progress Bar
#'
#' Create a new progress bar in your shiny app. This progress bar was designed
#' for apps that many pages that are viewed in a given order. This may be
#' useful apps that have a series of instructions screens or that are
#' designed for qualitative data collection (i.e., survey data). For example,
#' let's say that an app has 10 instruction pages. The user is instructed to
#' read through all pages and use the navigation buttons (next and back) to
#' move between pages. The progress bar can be useful in this situation as it
#' visually shows how many pages are left.
#'
#' @param start the starting point for the progress bar
#' @param min the minimum value of the progress bar
#' @param max the maximum value of the progress bar
#'
#'
#' @examples
#'
#' ```{r}
#' library(shiny)
#'
#' mybar <- accessibleshiny::progressbar(max = 10)
#'
#' ui <- tagList(
#'     accessibleshiny::use_accessibleshiny(),
#'     tags$main(
#'         style = "display: block; height: 50vh; margin-top: 50px",
#'         actionButton("increaseBar", "Next"),
#'         actionButton("descreaseBar", "Previous")
#'     ),
#'     mybar$bar(
#'         id = "bar1",
#'         fixed = TRUE
#'     )
#' )
#'
#' server <- function(input, output) {
#'     mybar$listen()
#'
#'     observeEvent(input$increaseBar, {
#'         mybar$increase()
#'     })
#'
#'     observeEvent(input$descreaseBar, {
#'         mybar$decrease()
#'     })
#' }
#'
#' shinyApp(ui, server)
#' ```
#' @return Create a new progress bar
#' @export
progressbar <- function(start = 0, min = 0, max = 7) {
    stopifnot(is.numeric(start))
    stopifnot(is.numeric(min))
    stopifnot(is.numeric(max))
    return(pbar$new(start = start, min = min, max = max))
}


#' R6 Class for progress bar
#'
#' @description R6 Class for progress bar
pbar <- R6::R6Class(
    classname = "shiny-progress-bar",
    public = list(

        #' @field elem ID of the progress bar defined by \code{bar()}
        elem = NULL,

        #' @field start the starting position for the progress bar
        start = NULL,

        #' @field current the current state of the progress bar
        current = NULL,

        #' @field min the minimum value of the progress bar
        min = NULL,

        #' @field max The maximum value of the progress bar
        max = NULL,

        #' @field text text formula that updates the `aria-valuetext`
        text = "{value} of {max}",

        #' @description
        #'
        #' Create a new progress bar
        #'
        #' @param start the starting progress
        #' @param min the minimum value of the progress bar
        #' @param max the maximum value of the progress bar
        #'
        #' @examples
        #' mybar <- progress(start = 0, min = 0, max = 10)
        #'
        #' @return Create a new progress bar
        initialize = function(start = 0, min = 0, max = 7) {
            stopifnot(is.numeric(start))
            stopifnot(is.numeric(min))
            stopifnot(is.numeric(max))
            self$start <- start
            self$current <- start
            self$min <- min
            self$max <- max
        },

        #' @description bar
        #'
        #' Create a new progress bar in the shiny UI
        #'
        #' @param inputId a unique identifier for the progress bar
        #' @param fill color used to style the progress bar
        #' @param fixed If `TRUE`, the progress bar will be fixed to the
        #'      top or bottom of the parent element
        #' @param position If `fixed = TRUE`, then the argument position
        #'      can be used to fix the progress bar to the "top" or "bottom" of
        #'      the parent element.
        #' @param yOffset A CSS value used to adjust the y position of the
        #'       progress bar relative to the parent element
        #' @param text formula for updating the aria text
        #' @param classnames string containing one or more css classes
        #'
        #' @examples
        #' ```{r}
        #' library(shiny)
        #'
        #' mybar <- accessibleshiny::progressbar(max = 10)
        #'
        #' ui <- tagList(
        #'     accessibleshiny::use_accessibleshiny(),
        #'     tags$main(
        #'         style = "display: block; height: 50vh; margin-top: 50px",
        #'         actionButton("increaseBar", "Next"),
        #'         actionButton("descreaseBar", "Previous")
        #'     ),
        #'     mybar$bar(
        #'         id = "bar1",
        #'         fixed = TRUE
        #'     )
        #' )
        #'
        #' server <- function(input, output) {
        #'     mybar$listen()
        #'
        #'     observeEvent(input$increaseBar, {
        #'         mybar$increase()
        #'     })
        #'
        #'     observeEvent(input$descreaseBar, {
        #'         mybar$decrease()
        #'     })
        #' }
        #'
        #' shinyApp(ui, server)
        #' ```
        bar = function(
            inputId,
            fill = NULL,
            fixed = FALSE,
            position = "top",
            yOffset = NULL,
            text = "{value} of {max}",
            classnames = NULL
        ) {
            stopifnot(is.logical(fixed))
            self$elem <- inputId

            # process fixed and position
            css <- "progressbar"
            if (isTRUE(fixed)) {
                css <- paste0(css, " progressbar__fixed")
                if (!position %in% c("top", "bottom")) {
                    stop("position is invalid. Enter 'top' or 'bottom'")
                } else {
                    css <- paste0(css, " position__", position)
                }
            }

            if (!is.null(classnames)) {
                css <- paste0(css, " ", classnames)
            }

            self$text <- text
            f <- private$update__ariatext()

            # build progress bar
            pb <- tags$div(
                    id = inputId,
                    class = css,
                    role = "progressbar",
                    `aria-valuecurrent` = self$current,
                    `aria-valuemin` = self$min,
                    `aria-valuemax` = self$max,
                    `aria-valuetext` = f,
                    tags$div(class = "bar")
            )

            # process background color
            if (length(fill) > 0) {
                pb$children[[1]]$attribs$style <- paste0(
                    "background-color: ", fill, ";"
                )
            }

            # process yOffset
            if (length(yOffset) > 0) {
                htmltools::validateCssUnit(yOffset)
                pb$attribs$style <- paste0(
                    pb$attribs$style,
                    "top: ", yOffset, ";"
                )
            }

            # return
            return(pb)
        },

        #' @description increase
        #'
        #' Increase the progress bar by 1 another number
        #'
        #' @param by a number between the min and max values (default = 1)
        #'
        #' @examples
        #'
        #' ```{r}
        #' library(shiny)
        #'
        #' mybar <- accessibleshiny::progressbar(max = 10)
        #'
        #' ui <- tagList(
        #'     accessibleshiny::use_accessibleshiny(),
        #'     tags$main(
        #'         style = "display: block; height: 50vh; margin-top: 50px",
        #'         actionButton("increaseBar", "Next")
        #'     ),
        #'     mybar$bar(
        #'         inputId = "bar1",
        #'         fixed = TRUE
        #'     )
        #' )
        #'
        #' server <- function(input, output) {
        #'     mybar$listen()
        #'
        #'     observeEvent(input$increaseBar, {
        #'         mybar$increase()
        #'     })
        #' }
        #'
        #' shinyApp(ui, server)
        #' ```
        increase = function(by = 1) {
            stopifnot(is.numeric(by))
            stopifnot(by > 0)

            # check to see if 'by' is out of bounds (only run if inbounds)
            if (!((by + self$current) > self$max)) {
                self$current <- self$current + by
                private$update__progressbar(current = self$current)
            }

            # when 'by' is out of bounds, reassign 'current' as 'max'
            if ((by + self$current) > self$max) {
                self$current <- self$max
            }
        },

        #' @description decrease
        #'
        #' Decrease the progress bar by 1 another number
        #'
        #' @param by A number between min and max values (default = 1)
        #'
        #' @examples
        #' ```{r}
        #' library(shiny)
        #'
        #' mybar <- accessibleshiny::progressbar(max = 10)
        #'
        #' ui <- tagList(
        #'     accessibleshiny::use_accessibleshiny(),
        #'     tags$main(
        #'         style = "display: block; height: 50vh; margin-top: 50px",
        #'         actionButton("descreaseBar", "Previous")
        #'     ),
        #'     mybar$bar(
        #'         inputId = "bar1",
        #'         fixed = TRUE
        #'     )
        #' )
        #'
        #' server <- function(input, output) {
        #'     mybar$listen()
        #'
        #'     observeEvent(input$descreaseBar, {
        #'         mybar$decrease()
        #'     })
        #' }
        #'
        #' shinyApp(ui, server)
        #' ```
        decrease = function(by = 1) {
            stopifnot(is.numeric(by))
            stopifnot(by > 0)

            # check to see if 'by' is out of bounds (only run if inbounds)
            if (!((self$current - by) < self$min)) {
                self$current <- self$current - by
                private$update__progressbar(current = self$current)
            }

            # when 'by' is out of bounds, reassign 'current' as 'min'
            if ((self$current - by) < self$min) {
                self$current <- self$min
            }
        },

        #' @description reset
        #'
        #' resets progress bar to its initial state
        #'
        #' @examples
        #' ```{r}
        #' library(shiny)
        #'
        #' mybar <- accessibleshiny::progressbar(max = 10)
        #'
        #' ui <- tagList(
        #'     accessibleshiny::use_accessibleshiny(),
        #'     tags$main(
        #'         style = "display: block; height: 50vh; margin-top: 50px",
        #'         actionButton("increaseBar", "Next"),
        #'         actionButton("resetBar", "Reset")
        #'     ),
        #'     mybar$bar(
        #'         inputId = "bar1",
        #'         fixed = TRUE
        #'     )
        #' )
        #'
        #' server <- function(input, output) {
        #'     mybar$listen()
        #'
        #'     observeEvent(input$increaseBar, {
        #'         mybar$increase()
        #'     })
        #'
        #'     observeEvent(input$resetBar, {
        #'         mybar$reset()
        #'     })
        #' }
        #'
        #' shinyApp(ui, server)
        #' ```
        reset = function() {
            self$current <- self$start
            private$update__progressbar(current = self$current)
        }
    ),

    # private functions
    private = list(

        # @description update aria text
        update__ariatext = function() {
            formula <- self$text
            if (grep("{value} of {max}", formula, fixed = TRUE)) {
                formula <- gsub("{value}", self$current, formula, fixed = TRUE)
                formula <- gsub("{max}", self$max, formula, fixed = TRUE)
            }
            return(formula)
        },

        # @description send data function
        # getDefaultReactiveDomain from shiny
        update__progressbar = function(current) {
            f <- private$update__ariatext()
            
            session <- getDefaultReactiveDomain()
            session$sendInputMessage(
                inputId = self$elem,
                message = list(
                    current = current,
                    text = f
                )
            )
        }
    )
)