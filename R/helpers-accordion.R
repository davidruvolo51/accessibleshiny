
# helper functions for collapsible sections (i.e., accordians)
accordion_helpers <- list()

# function to generate random ID
accordion_helpers$randomize_id <- function() {
    l <- sample(LETTERS, 5)
    n <- sample(seq_along(1:9), 5)
    return(paste0(sample(c(l, n), 9, replace = TRUE), collapse = ""))
}

# function to validate accordion options
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
        isOpen = FALSE
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
            if (!is.null(args$options$isOpen)) {
                options$isOpen <- args$options$isOpen
            }
        }
    }

    # return
    return(list(attr = attr, style = style, options = options))
}




# helper function for accordion header
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
    if (props$options$isOpen) {
        b$attribs$`aria-expanded` <- "true"
    }

    # append button to heading
    h$children <- b

    # return
    return(h)

}


# helper function for accordion content
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
    if (props$options$isOpen) {
        s$attribs$hidden <- "false"
        s$attribs$class <- "accordion-section"
    }
    return(s)
}