
# FUNCTION: datatable_helpers list object
datatable_helpers <- list()

# FUNCTION: build_header
datatable_helpers$build_header <- function(data, options) {
    columns <- colnames(data)
    cells <- lapply(seq_len(length(columns)), function(n) {

        # define cell content: as html or text
        if (isTRUE(options$asHTML)) {
            cell_value <- htmltools::HTML(columns[n])
        } else {
            cell_value <- columns[n]
        }

        # build header
        cell <- htmltools::tags$th(scope = "col", cell_value)
        return(cell)
    })

    # return header
    return(
        htmltools::tags$thead(
            htmltools::tags$tr(role = "row", cells)
        )
    )
}


# FUNCTION: build body
datatable_helpers$build_body <- function(data, options) {
    body <- lapply(seq_len(NROW(data)), function(row) {
        cells <- lapply(seq_len(NCOL(data)), function(col) {

            # process options: render as html or escape?
            if (isTRUE(options$asHTML)) {
                cell_value <- htmltools::HTML(data[row, col])
            } else {
                cell_value <- data[row, col]
            }

            # render css classes based on cell values and colnum
            cell_css <- paste0(
                datatable_helpers$cell_attributes(data[row, col]),
                " column-", col
            )

            # process options$rowHeaders (this generates the cell)
            if (isTRUE(options$rowHeaders) && col == 1) {
                cell <- htmltools::tags$th(
                    role = "rowheader",
                    class = cell_css
                )
            } else {
                cell <- htmltools::tags$td(
                    role = "cell",
                    class = cell_css
                )
            }

            # process options: responsive and rowHeaders
            if (isTRUE(options$responsive)) {
                cell$children <- list(
                    htmltools::tags$span(
                        class = "hidden-colname",
                        `aria-hidden` = "true",
                        colnames(data)[col]
                    ),
                    cell_value
                )
            } else {
                cell$children <- list(
                    cell_value
                )
            }

            # add data-attribute and return
            cell$attribs$`data-value` <- cell_value
            return(cell)
        })

        # return cells in a row
        return(htmltools::tags$tr(role = "row", cells))
    })

    # return body
    return(htmltools::tags$tbody(body))
}

# FUNCTION: evaluate data value by class type (returns a string of css classes)
datatable_helpers$cell_attributes <- function(value) {

    # standarize string output
    c <- class(value)
    class_string <- function(default = paste0("datatype-", c), new = NULL) {
        if (length(new) > 0) {
            paste0(default, " ", new)
        } else {
            default
        }
    }

    # format output classes (i.e., css classnames)
    # start by evaluating class types that should be handled
    # differently (i.e., specific processing, transformations, etc.).
    # Then, return things as normal
    if (c == "NULL") {
        out <- class_string(default = "datatype-null", new = "value-null")
    } else if (is.nan(value)) {
        out <- class_string(default = "datatype-nan", new = "value-nan")
    } else if (is.na(value) || value == "NA") {
        out <- class_string(default = "datatype-na", new = "value-na")
    } else if (c == "numeric") {
        if (value > 0) {
            out <- class_string(new = "value-positive")
        } else if (value < 0) {
            out <- class_string(new = "value-negative")
        } else if (value == 0) {
            out <- class_string(new = "value-zero")
        } else {
            out <- class_string(new = "value-unknown")
        }
    } else if (c == "logical") {
        out <- class_string(new = paste0("value-", tolower(value)))
    } else {
        out <- class_string()
    }

    # return
    return(out)
}

# add css dependency
datatable_helpers$datatable_dependencies <- function(...) {
    htmltools::htmlDependency(
        name = "datatable",
        version = "0.1.1",
        src = "assets/css/",
        package = "accessibleshiny",
        stylesheet = "datatable.min.css",
        all_files = FALSE
    )
}

# process html datatable classnames
datatable_helpers$datatable_css <- function(css, style) {
    default_class <- "datatable"
    out <- default_class
    if (isTRUE(style$rowHighlighting)) {
        out <- paste(out, "row-highlighting", sep = " ")
    }
    if (length(css) > 0) {
        out <- paste(out, css, sep = " ")
    }
    return(out)
}