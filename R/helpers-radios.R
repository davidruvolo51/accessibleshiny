# radio_helpers
# Combines helper functions in a list object
radio_helpers <- list()

# load html dependencies
radio_helpers$radio_dependencies <- function() {
    htmltools::htmlDependency(
        name = "radios",
        version = "0.1.0",
        src = "assets/css/",
        package = "accessibleshiny",
        stylesheet = "radios.min.css",
        all_files = FALSE
    )
}

# Build an internal data.frame to be used across helper functions. By default,
# values will get labels as it's not be applicable. If values are supplied,
# then the array should be added to the object `d`. This cuts out an additional
# evaluation that needs to be run. Inputs for `labels` and `values` should be
# of type array or as a single character string. The argument selected works
# in a similar manner. However, selected can only be an integer. By
# default, the function will assign `FALSE` to all entries and update the
# values accordingly.
radio_helpers$build_radio_df <- function(ids, labels, values, selected) {
    d <- data.frame(ids = ids, labels = labels, status = FALSE)
    if (length(values) > 0) d$values <- values
    if (length(selected) > 0 & is.numeric(selected)) {
        if (length(selected) > 1) {
            stop("Error in `radios`: `selected` must have a single integer")
        }
        d[selected, "status"] <- TRUE
    }
    return(d)
}

# The following function generates the html markup for radio inputs. It is
# designed to work with a single element (i.e., name, label, value) and return
# the <input> and <label> elements wrapped in a <div>. This will also add the
# checked status if TRUE. Use this function in the next function.
radio_helpers$build_radio_btn <- function(id, name, label, value, selected) {
    radio <- htmltools::tags$input(
        id = id,
        name = name,
        type = "radio",
        role = "radio",
        class = "radio-input"
    )
    if (length(value) > 0) radio$attribs$value <- value
    if (isTRUE(selected)) radio$attribs$checked <- "checked"
    label <- htmltools::tags$label(`for` = id, class = "radio-label", label)
    return(htmltools::tags$div(class = "radio-btn", radio, label))
}

# This function assembles a group of radio inputs based on the input
# dataset created from the function `build_radio_df`. The function will
# iterate over all items and update the id accordingly using `id` + `value`.
# It is recommeded that the user supplies a unique id and lowercase, no spaces
# in the values argument. An example of a "good" id and value is:
# id = "fruit", values = c("apples", "oranges", "limes", "lemons"). This will
# return the following ids for each element = "fruit-apples", "oranges"
radio_helpers$build_radio_group <- function(id, name, data, selected) {
    elems <- lapply(
        seq_len(NROW(data)), function(row) {
            radio_helpers$build_radio_btn(
                id = data[row, "ids"],
                name = name,
                label = data[row, "labels"],
                value = data[row, "values"],
                selected = data[row, "status"]
            )
        }
    )
    return(elems)
}