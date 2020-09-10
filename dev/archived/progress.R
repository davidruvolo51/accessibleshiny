#' \code{progress}
#'
#' @details # Progress Bars
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
#' @details ## Methods
#'
#' The primary function is progress. This function must be called outside
#' the shiny app. This enables you to use the component in the UI and server.
#' Assign the progress bar component to a new object, and then you can use
#' any of the following functions. For demonstration purposes, I will create
#' a new counter (mybar) and use it to explain the underlying methods.
#'
#' ```
#' mybar <- progress(start = 0, min = 0, max = 10)
#' ```
#'
#' * `mybar$bar(id = "mybar")`: This method generates the
#'          HTML markup for the progress bar element. A unique ID is
#'          required.
#' * `mybar$increase()`: This method increases the progress
#'          bar by 1 (default) or another value.
#' * `mybar$descrease()`: This method decreases the progress
#'          bar by 1 (default) or another value.
#' * `mybar$reset()`: This method resets the progress bar
#'          to the initial value (defined by `progress(...)) or other
#'          value.
#' * `mybar$print()`: This method prints all internal values
#'          (i.e., ID, min, max, start).
#'
#' These methods are described in the following sections.
#'
#' @details ### progress
#'
#' The function `progress` is used for initializing a new instance of the
#' progress bar component. Define a new object outside the shiny app; ideally,
#' in the `global.R` file or at the top of `app.R`. The `bar`
#' function takes the following parameters
#'
#' * `start`: the starting position of the progress bar
#' * `min`: the min value of the progress bar (i.e., floor)
#' * `max`: the max value of the progress bar (i.e., ceiling)
#'
#' ```{r}
#' mybar <- progress(max = 12)
#' ```
#'
#' @details ### bar
#'
#' In the shiny UI, you can create the HTML markup using the `bar`
#' function. There are a few styling and layout options that you can use. An
#' ID is required.
#'
#' * `id`: A unique ID for the progress bar
#' * `fill`: A background color for the progress bar
#' * `fixed`: logical value to set of the positioning of the progress bar
#' * `position`: if `fixed = TRUE`, then the bar can be placed at the "top" or
#'      "bottom" of the window (default is top).
#' * `yOffset`: a css value to offset the y position of the progress bar. This
#'      is useful if you want the progress bar to display directly below a
#'      navigation bar or another html element
#'
#' ```{r}
#' mybar$bar(id = "mybar", fixed = TRUE, position = "bottom")
#' ```
#'
#' @details ### listen
#'
#' In the shiny server, it is required to intialize the progress bar. Use the
#' following function in your server. There are no arguments for this method.
#'
#' ```
#' mybar$listen()
#' ```
#'
#' @details ### `increase()` and `decrease()`
#'
#' To update the progress bar you can use the functions `mybar$increase()`
#' or `mybar$decrease()`. Both functions take the following argument.
#'
#' * `by`: A value that adjusts the progress bar's counter. The default is 1.
#'      If you are using another value, it must be within the min and max
#'      values defined by `progress()`
#'
#' @details ### reset
#'
#' The method `reset()` can be used to reset the progress bar to its
#' initial state (defined by `progress`) or another value.
#'
#' *`to`: A numeric value that resets the progress bar. The
#'      default value 0, which is defined by the `progress` function.
#'      If you are using a new value, it must be within the min and max
#'      values.
#'
#' @details ### print
#'
#' Use the function `mybar$print()` to view the values associated with
#' your progress bar.
#'
#' @examples
#'
#' ```{r}
#' library(shiny)
#'
#' mybar <- accessibleshiny::progress(max = 10)
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
progress <- function(start = 0, min = 0, max = 7) {
    stopifnot(is.numeric(start))
    stopifnot(is.numeric(min))
    stopifnot(is.numeric(max))
    return(pbar$new(start = start, min = min, max = max))
}

#' \code{pbar} 
#' R6 Class for progress bar
#' @return R6 Class for progress bar
#' @export
pbar <- R6::R6Class(
    classname = "shiny-progress-bar",
    public = list(

        #' \code{elem}
        #' ID of the progress bar defined by \code{bar()}
        elem = NULL,

        #' \code{start}
        #' The starting position for the progress bar
        start = NULL,

        #' \code{current}
        #' The current state of the progress bar
        current = NULL,

        #' \code{min}
        #' The minimum value of the progress bar (i.e., floor)
        min = NULL,

        #' \code{max}
        #' The maximum value of the progress bar (i.e., ceiling)
        max = NULL,

        #' \code{new}
        #' Create a new progress bar
        #' @param start the starting progress
        #' @param min the minimum value of the progress bar
        #' @param max the maximum value of the progress bar
        initialize = function(start = 0, min = 0, max = 7) {
            stopifnot(is.numeric(start))
            stopifnot(is.numeric(min))
            stopifnot(is.numeric(max))
            self$start <- start
            self$current <- start
            self$min <- min
            self$max <- max
        },

        #' \code{bar}
        #' Create a new progress bar in the shiny UI
        #' @param id A unique ID for the progress bar
        #' @param fill A color used to style the progress bar
        #' @param fixed A logical value that is used to fix the progress to the
        #'      top or bottom of the parent element
        #' @param position If `fixed = TRUE`, then the argument position
        #'      can be used to fix the progress bar to the "top" or "bottom" of
        #'      the parent element.
        #' @param yOffset A CSS value used to adjust the y position of the
        #'       progress bar relative to the parent element
        bar = function(
            id = NULL, fill = NULL,
            fixed = FALSE, position = "top", yOffset = NULL
        ) {
            stopifnot(!is.null(id))
            stopifnot(is.logical(fixed))
            self$elem <- id

            # process fixed and position
            css <- "progress-bar-container"
            if (isTRUE(fixed)) {
                css <- paste0(css, " progress-bar-fixed")
                if (!position %in% c("top", "bottom")) {
                    stop("position is invalid. Enter 'top' or 'bottom'")
                } else {
                    css <- paste0(css, " position-", position)
                }
            }

            # build progress bar container
            b <- tags$div(
                id = paste0(id, "-container"),
                class = css
            )

            # build progress bar
            pb <- tags$div(
                    id = id,
                    class = "progress-bar",
                    role = "progressbar",
                    `aria-valuecurrent` = self$current,
                    `aria-valuemin` = self$min,
                    `aria-valuemax` = self$max,
                    `aria-valuetext` = self$text
            )

            # process background color
            if (length(fill) > 0) {
                #validateCssUnit(fill)
                pb$attribs$style <- paste0("background-color: ", fill, ";")
            }

            # process yOffset
            if (length(yOffset) > 0) {
                #validateCssUnit(yOffset)
                b$attribs$style <- paste0(
                    b$attribs$style,
                    "top: ", yOffset, ";"
                )
            }

            # bind pb to parent
            b$children <- pb

            # return
            return(b)
        },

        #' \code{listen}
        #' Initializes the progress bar within the shiny server
        listen = function() {
            private$init_parent_element(self$elem)
            private$update_progress_bar(self$elem, self$current, self$max)
        },

        #' \code{increase}
        #' Increase the progress bar by 1 another number
        #' @param by A number between min and max values (default = 1)
        increase = function(by = 1) {
            stopifnot(is.numeric(by))
            stopifnot(by > 0)

            # check to see if 'by' is out of bounds (only run if inbounds)
            if (!((by + self$current) > self$max)) {
                self$current <- self$current + by
                private$update_progress_bar(
                    id = self$elem,
                    current = self$current,
                    max = self$max
                )
            }

            # when 'by' is out of bounds, reassign 'current' as 'max'
            if ((by + self$current) > self$max) {
                self$current <- self$max
            }
        },

        #' \code{decrease}
        #' Decrease the progress bar by 1 another number
        #' @param by A number between min and max values (default = 1)
        decrease = function(by = 1) {
            stopifnot(is.numeric(by))
            stopifnot(by > 0)

            # check to see if 'by' is out of bounds (only run if inbounds)
            if (!((self$current - by) < self$min)) {
                self$current <- self$current - by
                private$update_progress_bar(
                    id = self$elem,
                    current = self$current,
                    max = self$max
                )
            }

            # when 'by' is out of bounds, reassign 'current' as 'min'
            if ((self$current - by) < self$min) {
                self$current <- self$min
            }
        },

        #' \code{reset}
        #' resets progress bar to its initial state
        #' @param to A numeric value that is used to reset progress bar
        #'     The default is the starting value, but can be another number
        #'     as long as it is within the min and max values of the progress
        #'     bar.
        reset = function(to = self$start) {
            stopifnot(is.numeric(to))

            # check to see if 'to' is out of bounds
            if (to < self$min) {
                stop("value 'to' is less than the min (", self$min, ")")
            } else if (to > self$max) {
                stop("value 'to' is greater than the max (", self$max, ")")
            } else {
                # reset and update
                self$current <- to
                private$update_progress_bar(
                    id = self$elem,
                    current = self$current,
                    max = self$max
                )
            }
        },

        # \code{print}
        #' Prints internal values of the progress bar
        print = function() {
            d <- structure(
                list(
                    elem = self$elem,
                    current = self$current,
                    min = self$min,
                    max = self$max
                ),
                class = "progress-bar-data"
            )
            print(d)
        }
    ),

    # private functions
    private = list(

        # init ui function
        # getDefaultReactiveDomain from shiny
        init_parent_element = function(id) {
            session <- getDefaultReactiveDomain()
            session$sendCustomMessage("init_parent_element", id)
        },

        # send data function
        # getDefaultReactiveDomain from shiny
        update_progress_bar = function(id, current, max) {
            session <- getDefaultReactiveDomain()
            session$sendCustomMessage(
                "update_progress_bar",
                list(id, current, max)
            )
        }
    )
)