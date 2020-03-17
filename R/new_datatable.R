
# pkgs
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(rlang))

# create data
set.seed(223)
d <- data.frame(
    xvar = round(rnorm(10, mean = 50, sd = 12.5),2),
    yvar = round(rnorm(10, mean = 50, sd = 12.5),2)
)

# define cell html attributes
set_cell_attributes <- function(index, value) {
    # set css classes based on class of value
    value_class <- class(value)
    attr <- list(
        class = paste0("datatype-", tolower(value_class)),
        "data-value" = ""
    )

    # class === numeric
    if (value_class == "numeric") {
        if (value > 0) {
            attr$class <- "datatype-numeric value-positive"
            attr$`data-value` <- value
        }
        if (value < 0) {
            attr$class <- "datatype-numeric value-negative"
            attr$`data-value` <- value
        }
        if (value == 0) {
            attr$class <- "datatype-numeric value-zero"
        }
    }

    # class === bool
    if (value_class == "logical") {
        # true
        if (isTRUE(value)) {
            attr$class <- "datatype-logical value-true"
            attr$`data-value` <- value
        }

        # false
        if (isFALSE(value)) {
            attr$class <- "datatype-logical value-false"
            attr$`data-value` <- value
        }
    }

    # update css with index
    attr$class <- paste0("column-", index, " ", attr$class)
    return(attr)
}

# test
set_cell_attributes(1, "test")
set_cell_attributes(1, 1245)
set_cell_attributes(1, FALSE)

# generate markup for table body cells
tbody_cells <- function(...) {
    args <- list2(...)
    index <- 1
    cell <- imap(args, function(d, .x) {
        if (index == 1) {
            c <- tags$th(d)
        }
        if (index > 1) {
            c <- tags$td(d)
        }

        # run cell attributes
        attr <- set_cell_attributes(index, d)

        # attach css class
        c$attribs$class <- attr[["class"]]

        # add data-value attribute if applicable
        if (attr[["data-value"]] != "") {
            c$attribs$`data-value` <- attr[["data-value"]]
        }

        # update rols
        c$attribs$role <- "cell"

        # index++
        index <<- index + 1

        return(c)
    })
    return(cell)
}

tbody_rows <- function(...) {
    args <- list2(...)
    cells <- pmap(args, tbody_cells)
    r <- tags$tr(cells, role = "row")
    return(r)
}

tbody <- function(data) {
    body <- pmap(data, tbody_rows)
    return(tags$tbody(body))
}

# test
tbody(d)


thead <- function(data) {
    index <- 1
    headers <- map(names(data), function(c) {
        col <- as.character(c)
        c <- tags$th(c)
        c$attribs$class <- paste0(
            "column-", index,
            " colname-", col
        )
        c$attribs$role <- "cell"
        c$attribs$scope <- "col"
        index <<- index + 1
        return(c)
    })
    row <- tags$tr(role = "row", headers)
    return(tags$thead(row))
}

thead(d)

# validate props functions
validate_props <- function(...) {
    # process three dots
    args <- dots_list(...)

    # set defaults
    attr <- list(class = "datatable row_highlighting")
    options <- list(
        responsive = TRUE,
        row_headers = FALSE,
        html = FALSE
    )

    # evaluate args only if args exist
    if (length(args) > 0) {

        # add id and/or css
        if (!is.null(args$id)) attr$id <- args$id
        if (!is.null(args$class)) {
            attr$class <- paste0(attr$class, " ", args$class)
        }

        # process style args (i.e., update css)
        if (!is.null(args$style)) {
            if (!is.null(args$style$row_highlighting)) {
                if (isFALSE(args$style$row_highlighting)) {
                    attr$class <- gsub(" row_highlighting", "", attr$class)
                }
            }
        }

        # options
        if (!is.null(args$options)) {
            if (!is.null(args$options$row_headers)) {
                options$row_headers <- args$options$row_headers
            }
            if (!is.null(args$options$html)) {
                options$html <- args$options$html
            }
            if (!is.null(args$options$responsive)) {
                options$responsive <- args$options$responsive
            }
        }
    }

    # return
    return(list(attribs = attr, options = options))
}


# primary function
tbl <- function(data, caption = NULL, ...) {

    # validate input args
    props <- validate_props(...)

    # generate table markup
    tbl <- tags$table(
        thead(data),
        tbody(data)
    )

    # update css
    tbl$attribs <- props$attribs

    # append caption
    if (length(caption) > 0) {
        tbl$children <- list(
            tags$caption(caption),
            tbl$children
        )
    }

    # return
    return(tbl)
}


tbl(
    data = d,
    caption = "hello, world!",
    id = "test",
    class = "dark-theme",
    style = list(
        row_highlighting = TRUE
    ),
    options = list(
        responsive = FALSE
    )
)