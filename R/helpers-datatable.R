
# "modularize" datatable helpers
# All helper functions and common variables will be nested into the object
# `datatable_helpers`. See the wiki for more information about each sub-
# element and how these functions are used.
datatable_helpers <- list()

# The following functions are structured in the following order.
#   1. Component Defaults
#   2. Validation Function
#   3. Loading Dependency
#   4. Defining Component Attributes (i.e., class and id)
#   5. Generating HTML Elements
#
#//////////////////////////////////////////////////////////////////////////////

# ~ 0 ~
# DEFINE COMPONENT DEFAULTS & VALIDATION PROPS
# The default items replace named arguments and process optional arguments
# passed through `...`. The two elements that are currently accepted are
# `style` and `options`.

datatable_helpers$defaults <- list()

# defaults$style
# define a list of default settings for the optional argument `style`. The
# options defined here are used to control the rendering of the css props
# of the component. For example, if the user wants to display all rendering
# of css styles, they can do so by passing `style = list(enabled = FALSE)`
# in the main function. Add more props as needed and make sure the validation
# function is updated accordingly.
datatable_helpers$defaults$style <- list(
    rowHighlighting = TRUE
)

# defaults$options
# define a list of default settings that control the html markup of the ui
# component. These arguments are passed as a list object through `...`. This
# allows the user to manipulate the component inline. More information can
# be found in the wiki. Add more props as needed and make sure the validation
# function is updated accordingly.
datatable_helpers$defaults$options <- list(
    responsive = TRUE,
    rowHeaders = TRUE,
    asHTML = FALSE,
    loadDependency = TRUE
)

# VALIDATE PROPS
# The following function validates the input arguments in `...` and substitute
# defaults or replaces missing values with the defaults.
datatable_helpers$validate_props <- function(...) {

    # process inputs and prep options list
    args <- eval(substitute(alist(...)))
    inputs <- purrr::map(args, as.list)
    props <- list()

    #//////////////////////////////////////

    # PROCESS INPUTS$STYLE
    # when missing
    if (is.null(inputs$style)) {
        props$style <- datatable_helpers$defaults$style
    }

    # when not missing
    if (!is.null(inputs$style)) {

        # assign inputs$style to props$style
        props$style <-  inputs$style

        # is option `rowHighlighting` missing?
        if (is.null(inputs$style$rowHighlighting)) {
            props$style$rowHighlighting <-
                datatable_helpers$defaults$style$rowHighlighting
        }
    }

    #//////////////////////////////////////

    # PROCESS INPUT$OPTIONS
    # when missing
    if (is.null(inputs$options)) {
        props$options <- datatable_helpers$defaults$options
    }

    # when not missing
    if (!is.null(inputs$options)) {

        # assign inputs$options to props$options
        props$options <- inputs$options

        # is option `responsive` missing?
        if (is.null(inputs$options$responsive)) {
            props$options$responsive <-
                datatable_helpers$defaults$options$responsive
        }
        # is option `rowHeading` missing?
        if (is.null(inputs$options$rowHighlighting)) {
            props$options$rowHighlighting <-
                datatable_helpers$defaults$options$rowHighlighting
        }
        # is option `asHTML` missing?
        if (is.null(inputs$options$asHTML)) {
            props$options$asHTML <-
                datatable_helpers$defaults$options$asHTML
        }
    }

    # RETURN PROPS
    # output will be as list, use *$style and *$options to extract
    # the user defined inputs and component defaults
    return(props)
}

#//////////////////////////////////////////////////////////////////////////////

# ~ 1 ~
# DEFINE FUNCTIONS THAT CONTROL CSS ATTRIBUTES OF THE COMPONENT
# The following functions are used to update css attributes of html elements.
# For example, the set_cell_css function is used to apply classnames at the
# cell level which can be used for further formating in css. See the wiki
# for more information about each helper function

# set_table_attributes
# This function processes the input arguments, `id`, `css` and `style` to
# define the attributes for the parent element, ie., <table>. This returns
# a list object which can be added as a one stop attribute generator.
datatable_helpers$set_table_attributes <- function(id, css, style) {
    attributes <- list()
    # apply default css
    attributes$class <- "datatable"

    # should the row highlighting class be added?
    if (isTRUE(style$rowHighlighting)) {
        attributes$class <- paste(
            attributes$class,
            "row-highlighting",
            sep = " "
        )
    }

    # Did the user supply a string containing css classes?
    if (length(css) > 0) {
        attributes$class <- paste(
            attributes$class,
            css,
            sep = " "
        )
    }

    # apply id
    if (length(id) > 0) attributes$id <- id

    # return as list object
    return(attributes)
}

# set_cell_css
# This function evaluates a value and returns a character string containing
# one or more css classes. By using the the base `class` function, the
# function builds css classes that can be used to further customize tables
# using css. For example, if the cell value in column 2 is a `50.12`, the
# function will return: "datatype-number value-positive column-2".
# This will allow users to easily format numbers in css. See comments for
# further outputs.
datatable_helpers$set_cell_css <- function(value) {

    # standarize string output this inner function standarizes the css classes
    # If the prefixes need to be changed, this will only need to be changed
    # once.
    c <- class(value)
    class_string <- function(default = paste0("datatype-", c), new = NULL) {
        if (length(new) > 0) {
            paste0(default, " ", new)
        } else {
            default
        }
    }

    # format output classes (i.e., css classnames) start by evaluating class
    # types that should be handled differently (i.e., specific processing,
    # transformations, etc.). Then, return things as normal. The structure of
    # the string output is: 'datatype-class [other classes if applicable]'.
    if (c == "NULL") {

        # if input class is NULL, return: "datatype-null value-null"
        out <- class_string(default = "datatype-null", new = "value-null")

    } else if (is.nan(value)) {

        # if input class is NaN, return: "datatype-nan value-nan"
        out <- class_string(default = "datatype-nan", new = "value-nan")

    } else if (is.na(value) || value == "NA") {

        # if class is NA, return: "datatype-na value-na"
        out <- class_string(default = "datatype-na", new = "value-na")

    } else if (c == "numeric") {

        # if class is numeric, return: "datatype-numeric value-*"
        if (value > 0) {

            # if value is positive, return: "datatype-numeric value-positive"
            out <- class_string(new = "value-positive")

        } else if (value < 0) {

            # if value is negative, return: "datatype-numeric value-negative"
            out <- class_string(new = "value-negative")

        } else if (value == 0) {

            # if value is zero, return: "datatype-numeric value-zero"
            out <- class_string(new = "value-zero")

        } else {

            # if something else, return: "datatype-numeric value-unknown"
            out <- class_string(new = "value-unknown")

        }
    } else if (c == "logical") {

        # if value is logical, return: "datatype-logical value-*"
        # where "value-*" is either: "value-true" or "value-false"
        out <- class_string(new = paste0("value-", tolower(value)))

    } else {

        # for all else, return the class name: "datatype-*"
        # where "*" is the result of the function `class`
        out <- class_string()
    }

    # return
    return(out)
}

#//////////////////////////////////////////////////////////////////////////////

# ~ 2 ~
# DEFINE FUNCTIONS USED TO BUILD TABLE ELEMENTS
# The following functions are used to build table elements (header, body, cells)
# These functions may use utilize the component defaults sublist defined in
# section 0.

# build_header
# define a function that generates a table header element from
# an input dataset and renders with the options object. At the
# moment, this function processes the option `asHTML` which,
# when TRUE, will render column headers as html. This is
# useful for situtations where you want to create a clickable
# item, insert line breaks, etc.
datatable_helpers$build_header <- function(data, options) {
    columns <- colnames(data)
    cells <- lapply(seq_len(length(columns)), function(n) {

        # define cell content: as html or text
        if (isTRUE(options$asHTML)) {
            cell_value <- htmltools::HTML(columns[n])
        } else {
            cell_value <- columns[n]
        }

        # assemble table header cell
        cell <- htmltools::tags$th(
            scope = "col",
            class = paste0("column-", n),
            cell_value
        )
        return(cell)
    })

    # assemble table header element
    return(
        htmltools::tags$thead(
            class = "datatable-header",
            htmltools::tags$tr(role = "row", cells)
        )
    )
}

# build_body
# define a function that generates the html markup for the table body element
# (<tbody>). This function works by iterating over the input data by row and
# then by columns. The function is structured this way as the markup will also
# need to consider a number of options that effect the cell content. To
# manipulate the cell level content, this function uses nested lapply functions.
# The parent lapply iterates by row and the second iterates by column. The first
# evaluation is to determine if the inner cell content should be rendered as an
# html element or as raw text (this is the most important and should be done
# first). Second, using the cell value, the function returns the appropriate
# css for a each cell (see comment for the function `set_cell_css`). Next,
# using the option `rowHeaders`, the function will return the cell markup (
# either a row header or a normal cell) with the css classes. If the option
# `responsive` is true, the function will append the cell content with an
# inline span element that is displayed/hidden based on viewport sizes. When
# the cell is built, the final functions group cells into html rows and then
# into the table body element.
datatable_helpers$build_body <- function(data, style, options) {
    body <- lapply(seq_len(NROW(data)), function(row) {
        cells <- lapply(seq_len(NCOL(data)), function(col) {

            # process options: render as html or escape?
            if (isTRUE(options$asHTML)) {
                cell_value <- htmltools::HTML(data[row, col])
            } else {
                cell_value <- data[row, col]
            }

            # process options$rowHeaders (this generates the cell)
            if (isTRUE(options$rowHeaders) && col == 1) {
                cell <- htmltools::tags$th(
                    role = "rowheader"
                )
            } else {
                cell <- htmltools::tags$td(
                    role = "cell"
                )
            }

            # apply cell css
            cell$attribs$class <- paste0(
                datatable_helpers$set_cell_css(data[row, col]),
                    " column-", col
            )

            # add custom data attrib and responsiveness (if requested)
            cell$attribs$`data-value` <- cell_value
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
                cell$children <- list(cell_value)
            }

            # return cell
            return(cell)
        })

        # return cells in a row
        return(htmltools::tags$tr(role = "row", cells))
    })

    # return body
    return(htmltools::tags$tbody(class = "datatable-body", body))
}