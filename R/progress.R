#' \code{Progress}
#'
#' Create a new R6 progress bar.
#' @return Create a new R6 progress bar
#' @examples
#' appProgress <- pbars::progress$new(min = 0, max = 12)
#' ui <- tagList(
#'      ...
#'      appProgress$bar(
#'          id = "complete",
#'          fixed = TRUE,
#'          position = "bottom",
#'          fill = "#006992"
#'      ),
#'      ...,
#'      tags$button(
#'          id = "nextPage",
#'          class = "action-button shiny-bound-input",
#'          "Next Page"
#'      )
#' )
#' server <- function(input, output, session) {
#'      appProgress$init()
#'      observeEvent(input$nextPage, {
#'          appProgress$increase()
#'      })
#' }
#' shinyApp(ui, server)
#' @export
progress <- R6Class(
    classname = "shiny-progress-bar",
    public = list(

        #' @section Internal Values
        #' Internal values that can be used
        #' to control progress bars
        elem = NULL,
        start = NULL,
        current = NULL,
        min = NULL,
        max = NULL,

        #' @param start the starting value for the progress bar
        #' @param min the minimum value for the progress bar (default is 0)
        #' @param max the maxium value for the progress bar (default is 7)
        initialize = function(start = 0, min = 0, max = 7) {
            stopifnot(is.numeric(start))
            stopifnot(is.numeric(min))
            stopifnot(is.numeric(max))
            self$start <- start
            self$current <- start
            self$min <- min
            self$max <- max
        },


        #' @param id the id of the progress bar
        #' @param fixed logical value to set of the positioning of the pbar
        #' @param position if \code{fixed = TRUE}, the bar can be placed at the
        #'                top or bottom of the window (default is top)
        #' @param fill a color of the progress bar (default is #bdbdbd)
        bar = function(id = NULL, fixed = FALSE, position = "top", fill = NULL) {
            stopifnot(!is.null(id))
            stopifnot(is.logical(fixed))

            # set id to state
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

            # generate html markup using tags from htmltools
            b <- tags$div(
                id = paste0(id, "-container"),
                class = css,
                tags$div(
                    id = id,
                    class = "progress-bar",
                    role = "progressbar",
                    `aria-valuecurrent` = self$current,
                    `aria-valuemin` = self$min,
                    `aria-valuemax` = self$max,
                    `aria-valuetext` = self$text
                )
            )

            # add color
            if (length(fill) > 0) {
                b$attribs$style <- paste0("background-color: ", fill, ";")
            }

            # return
            return(b)
        },

        #' \code{init}
        #' Initializes Progress Bar server-side
        #' @param id the id of the progress bar
        init = function(id = self$elem) {
            private$init_parent_element(id)
            private$update_progress_bar(id, self$current, self$max)
        },

        #' \code{increase}
        #' Increase the internal counter by 1 or another number
        #' @param id the id of the progress bar
        #' @param by the number to increases by (default 1)
        increase = function(id = self$elem, by = 1) {
            stopifnot(!is.null(id))
            stopifnot(is.numeric(by))
            stopifnot(by > 0)

            # check to see if 'by' is out of bounds (only run if inbounds)
            if (!((by + self$current) > self$max)) {
                self$current <- self$current + by
                private$update_progress_bar(
                    id = id,
                    current = self$current,
                    max = self$max
                )
            }

            # when 'by' is out of bounds, reassign 'current' as 'max'
            if ((by + self$current) > self$max) {
                self$current <- self$max
            }
        },

        #' \code{descrease}
        #' Decreases the internal counter by 1 or another number
        #' @param id the id of the progress bar
        #' @param by the number to decreases by (default 1)
        decrease = function(id = self$elem, by = 1) {
            stopifnot(!is.null(id))
            stopifnot(is.numeric(by))
            stopifnot(by > 0)

            # check to see if 'by' is out of bounds (only run if inbounds)
            if (!((self$current - by) < self$min)) {
                self$current <- self$current - by
                private$update_progress_bar(
                    id = id,
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
        #' Reset a progress bar to start or another value
        #' @param id the id of the progress bar
        #' @param to the value to reset the bar to (defaults to min value or 0)
        reset = function(id = self$elem, to = self$start) {
            stopifnot(!is.null(id))
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
                    id = id,
                    current = self$current,
                    max = self$max
                )
            }
        },

        #' \code{print}
        #' Print internal values
        print = function() {
            d <- structure(
                list(
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