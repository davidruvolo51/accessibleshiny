
# accordion_helpers
# Create a series of secondary functions that generate child elements of the
# accordion and process arguments. These functions are added to a nested list
# object that is called inthe main function.
accordion_helpers <- list()

# randomize_id
# A function that creates a randomised ID is needed for default behavior. This
# will make the selection of elements in JavaScript a bit easier. Users can
# also use the ID in css as it is also added into the `data-group` attribute
# for all accordion elements. The following function randomizes the order of
# 5 randomly selected letters and numbers. If the user, supplies their own
# ID, then this function is not run.
accordion_helpers$randomize_id <- function() {
    l <- sample(LETTERS, 5)
    n <- sample(seq_along(1:9), 5)
    return(paste0(sample(c(l, n), 9, replace = TRUE), collapse = ""))
}

# validate_props
# Define a function that processes all optional arguments. In the primary
# function, the user enters optional arguments using the ellipses `...`.
# Optional arguments are gathered using the list2() function from the
# rlang package. The returned object is a list that contains all html
# attributes, styles, and options. This list is used in other secondary
# functions.
accordion_helpers$validate_props <- function(...) {
    args <- list2(...)

    # set defaults
    attr <- list(
        id = accordion_helpers$randomize_id()
    )
    style <- list(
        background_fill = "#3F454B",
        icon_fill = "#ffffff"
    )
    options <- list(
        heading_level = "h4",
        start_open = FALSE,
        print_id = FALSE
    )

    # process all arguments
    if (!is.null(args)) {

        # process id
        if (!is.null(args$id)) {
            attr$id <- args$id
        }

        # process style
        if (!is.null(args$style)) {

            # background fill for icon
            if (!is.null(args$style$icon_background)) {
                style$background_fill <- args$style$background_fill
            }

            # fill for icon symbol
            if (!is.null(args$style$icon_fill)) {
                stle$icon_fill <- args$style$icon_fill
            }
        }

        # process options
        if (!is.null(args$options)) {

            # process heading level
            if (!is.null(args$options$heading_level)) {
                html_headings <- c("h1", "h2", "h3", "h4", "h5", "h6")
                heading_level <- args$options$heading_level
                if (!heading_level %in% html_headings) {
                    msg <- paste0(
                        "heading '",
                        heading_level,
                        "' does not exist. Use h1 through h6"
                    )
                   stop(msg)
                } else {
                    options$heading_level <- heading_level
                }
            }

            # process default state
            if (!is.null(args$options$start_open)) {
                options$start_open <- args$options$start_open
            }

            # process print_id function
            if (!is.null(args$options$print_id)) {
                options$print_id <- args$options$print_id
            }
        }
    }

    # return
    return(list(attr = attr, style = style, options = options))
}

# heading
# Define a function that returns the heading of the accordion section. This
# function returns a heading element (<h4>) that places the title in a button.
# The button is used to open and close the hidden section. This function is
# used in the primary function along with the `content()` function. The
# input arguments are:
#   - title: user defined title
#   - props: the processed arguments generated from the validate_props function
# The returned object is an html element.
accordion_helpers$heading <- function(title, props) {

    # build heading element
    h <- tags[[props$options$heading_level]](
        class = "accordion-heading",
        `data-group` = props$attr$id
    )

    # build button
    b <- tags$button(
        id = paste0("accordion-btn-", props$attr$id),
        `data-group` = props$attr$id,
        `aria-controls` = "accordion-panel",
        `aria-expanded` = "false",
        title,
        icons$plus(
            id = paste0("icon-", props$attr$id),
            class = "accordion-icon",
            background_fill = props$style$background_fill,
            icon_color = props$style$icon_fill
        )
    )

    # process state
    if (props$options$start_open) {
        b$attribs$`aria-expanded` <- "true"
    }

    # append button to heading
    h$children <- b

    # return
    return(h)

}

# content
# Define a function that creates the collapsible element in the accordion.
# This function is used in the primary function along with the `heading()`
# function. This function takes the following arguments
#    - html: user defined html element or tagList of elements
#   - props: the processed arguments generated from the validate_props function
# The returned object is an html element.
accordion_helpers$content <- function(html, props) {
    s <- tags$section(
        id = paste0("accordion-section-", props$attr$id),
        class = "accordion-section accordion-hidden",
        `data-group` = props$attr$id,
        `aria-labelledby` = paste0("accordion-btn-", props$attr$id),
        hidden = "true",
        html
    )
    # process open state
    if (props$options$start_open) {
        s$attribs$hidden <- "false"
        s$attribs$class <- "accordion-section"
    }
    return(s)
}

# print_id
# Define a function that prints the randomized ID or the user defined ID. This
# would be useful when selecting and styling elements in CSS. This function
# prints a message in the console or terminal. This function is used in the
# primary function and processed only if the value is TRUE. This function takes
# the user supplied title and the ID.
accordion_helpers$print_id <- function(title, id) {
    msg <- paste0(
        "The ID of the accordion element titled '", as.character(title),
        "' is:", id, "This ID is added to the `data-group`",
        "attribute of all elements beloning to this accordion. To select",
        "an element in CSS, use: \n",
        ".accordion-header[data-group='", id, "'] {\n",
        "\t...",
        "}\n",
        "See reference for a list of CSS classes used in this element."
    )
    cat(msg)
}