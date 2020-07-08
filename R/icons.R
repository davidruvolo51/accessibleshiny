#' SVG Icons
#'
#' A set of svg icons
#' @name icons
#' @return A set of SVG icons
#' @keywords icons
#' @export
icons <- list()


#' \code{checkmark}
#' @name icon$checkmark
#' @usage icon$checkmark()
#' @return Create an svg icon of a checkmark
#' @param id a unique ID to be applied to the svg element
#' @param class a css class to be applied to the svg element
#' @param background_fill a color to be applied to the inner svg element
#' @param icon_color a color to be applied to the inner svg element
#' @param aria_hidden a logical value for hiding elements
#'      (Useful for purely aesthetic elements)
#' @keywords icons checkmark
#' @importFrom htmltools tag tagList
#' @export
icons$checkmark <- function(
    id = NULL,
    class = NULL,
    background_fill = "#f6f6f6",
    icon_color = "#3f454b",
    data_group = NULL,
    aria_hidden = TRUE
) {
    # validate
    stopifnot(is.logical(aria_hidden))

    # parent
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "aria-hidden" = tolower(as.character(aria_hidden)),
            "class" = "accessibleshiny-icon icon-checkmark",
            "width" = "25",
            "height" = "25",
            "viewBox" = "0 0 25 25",
            "version" = "1.1",
            "xmlns" = "http://www.w3.org/2000/svg",
            "xmlns:xlink" = "http://www.w3.org/1999/xlink"
        )
    )

    # append id, class, data_group
    if (!is.null(id)) svg$attribs$id <- id
    if (!is.null(data_group)) svg$attribs$`data-group` <- data_group
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }

    # append children
    svg$children <- tagList(
        tag(
            `_tag_name` = "circle",
            list(
                "fill" = background_fill,
                "cx" = "12.5",
                "cy" = "12.5",
                "r" = "12.5"
            )
        ),
        tag(
            `_tag_name` = "polyline",
            list(
                "fill" = "none",
                "stroke" = icon_color,
                "stroke-width" = "2",
                "stroke-linecap" = "round",
                "stroke-linejoin" = "round",
                "points" = "6,13 11.5,19 18.7,6.5"
            )
        )
    )

    # return
    return(svg)
}


#' \code{chevron}
#' @name icon$chevron
#' @usage icon$chevron
#' @return An svg chevron
#' @param id a unique ID to be applied to the svg element
#' @param class a css class to be applied to the svg element
#' @param icon_fill a color that fills the icon shape
#'           (useful when closed = TRUE)
#' @param icon_color a color to be applied to the inner svg element
#' @param closed a logical arg that created a closed shape (default: false)
#' @param direction a value that specifies the direction the chevron points to
#'          ("bottom", "top", "left", "right")
#' @param data_group a custom data attribute
#' @param aria_hidden a logical value for hiding elements
#'      (Useful for purely aesthetic elements)
#' @keywords icons chevron
#' @importFrom htmltools tag tagList
#' @export
icons$chevron <- function(
    id = NULL,
    class = NULL,
    icon_fill = "none",
    icon_color = "#3f454b",
    closed = FALSE,
    direction = "bottom",
    data_group = NULL,
    aria_hidden = TRUE
) {

    # directions + rotate coords
    coords <- list(
        "top" = "180,10,10",
        "right" = "270,10,10",
        "bottom" = "0,10,10",
        "left" = "90,10,10"
    )
    # validate
    stopifnot(is.logical(closed))
    stopifnot(is.logical(aria_hidden))
    stopifnot(is.character(direction))
    if (!tolower(direction) %in% names(coords)) {
        stop(
            paste0(
                "direction not valid. Use",
                paste0(names(coords), collapse = ", ")
            )
        )
    }

    # build svg element
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "aria-hidden" = tolower(as.character(aria_hidden)),
            "class" = paste0(
                "accessibleshiny-icon icon-chevron icon-chevron-",
                tolower(direction)
            ),
            "width" = "20",
            "height" = "20",
            "viewBox" = "0 0 20 20",
            "version" = "1.1",
            "xmlns" = "http://www.w3.org/2000/svg",
            "xmlns:xlink" = "http://www.w3.org/1999/xlink"
        )
    )

    # append id, class, data_group
    if (!is.null(id)) svg$attribs$id <- id
    if (!is.null(data_group)) svg$attribs$`data-group` <- data_group
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }

    # process data points
    points <- "2,10 10,18 18,10"
    if (closed) points <- paste0(points, " 2,10")

    # append children
    svg$children <- tag(
        `_tag_name` = "g",
        list(
            "transform" = paste0("rotate(", coords[[direction]], ")"),
            tag(
                `_tag_name` = "polyline",
                list(
                    "fill" = icon_fill,
                    "stroke" = icon_color,
                    "stroke-width" = "2",
                    "stroke-linecap" = "round",
                    "stroke-linejoin" = "round",
                    "points" = points
                )
            )
        )
    )

    # return
    return(svg)
}

#' \code{plus}
#' A plus sign with a filled background
#' @name icons$plus
#' @usage icons$plus()
#' @return A plus sign with a filled background
#' @param id a unique ID to be applied to the svg element
#' @param class a css class to be applied to the svg element
#' @param background_fill a color to be applied to the inner svg element
#' @param icon_color a color to be applied to the inner svg element
#' @param data_group a custom data attribute for use in the accordion function
#' @param aria_hidden a logical value for hiding elements
#'      (Useful for purely aesthetic elements)
#' @keywords icons plus
#' @importFrom htmltools tag tagList
#' @export
icons$plus <- function(
    id = NULL,
    class = NULL,
    background_fill = "#f6f6f6",
    icon_color = "#3f454b",
    data_group = NULL,
    aria_hidden = TRUE
    ) {

    # validate
    stopifnot(is.logical(aria_hidden))

    # build svg element
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "aria-hidden" = tolower(as.character(aria_hidden)),
            "class" = "accessibleshiny-icon icon-plus",
            "width" = "25",
            "height" = "25",
            "viewBox" = "0 0 25 25",
            "version" = "1.1",
            "xmlns" = "http://www.w3.org/2000/svg",
            "xmlns:xlink" = "http://www.w3.org/1999/xlink"
        )
    )

    # append id, class, data_group
    if (!is.null(id)) svg$attribs$id <- id
    if (!is.null(data_group)) svg$attribs$`data-group` <- data_group
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }


    # build plus sign using two svg line elements
    svg$children <- tagList(
        # circle for filled background
        tag(
            `_tag_name` = "circle",
            list(
                "class" = "icon-plus-background",
                "fill" = background_fill,
                "cx" = "12.5",
                "cy" = "12.5",
                "r" = "12.5"
            )
        ),

        # vertical line
        tag(
            `_tag_name` = "line",
            list(
                "class" = "icon-plus-symbol icon-plus-line",
                "stroke" = icon_color,
                "stroke-linecap" = "round",
                "stroke-width" = "2",
                "x1" = "12.5",
                "y1" = "5",
                "x2" = "12.5",
                "y2" = "20"
            )
        ),

        # horizontal lines
        tag(
            `_tag_name` = "line",
            list(
                "class" = "icon-plus-symbol icon-plus-line",
                "stroke" = icon_color,
                "stroke-linecap" = "round",
                "stroke-width" = "2",
                "x1" = "5",
                "y1" = "12.5",
                "x2" = "20",
                "y2" = "12.5"
            )
        )
    )

    # return
    return(svg)
}

#' \code{restart}
#' @return create a restart icon
#' @name icons$restart
#' @usage icons$restart()
#' @param id a unique ID to be applied to the svg element
#' @param class a css class to be applied to the svg element
#' @param icon_color a color to be applied to the inner svg element
#' @param data_group a custom data attribute useful for subgroups of icons
#' @param aria_hidden a logical value for hiding elements
#'      (Useful for purely aesthetic elements)
#' @keywords icons restart resfresh
#' @importFrom htmltools tag tagList
#' @export
icons$restart <- function(
    id = NULL,
    class = NULL,
    icon_color = "#3f454b",
    data_group = NULL,
    aria_hidden = TRUE
) {
    # validate
    stopifnot(is.logical(aria_hidden))

    # svg
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "class" = "accessibleshiny-icon icon-restart",
            "width" = "50",
            "height" = "50",
            "viewBox" = "0 0 50 50",
            "aria-hidden" = tolower(as.character(aria_hidden)),
            "version" = "1.1",
            "xmlns" = "http://www.w3.org/2000/svg",
            "xmlns:xlink" = "http://www.w3.org/1999/xlink"
        )
    )

    # append id, class, data_group
    if (!is.null(id)) svg$attribs$id <- id
    if (!is.null(data_group)) svg$attribs$`data-group` <- data_group
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }

    # append children
    svg$children <- tagList(
        tag(
            `_tag_name` = "path",
            list(
                "class" = "icon-restart-symbol",
                "stroke" = icon_color,
                "stroke-width" = "6",
                "fill" = "none",
                "d" = paste0(
                    "M40.9982944,33.634926 C42.2766089,31.2045528",
                    " 43,28.4367549 43,25.5 C43,22.396187",
                    " 42.1919675,19.4811002 40.7747866,16.9536235",
                    " C37.7789988,11.6107743 32.0611702,8 25.5,8",
                    " C15.8350169,8 8,15.8350169 8,25.5 C8,35.1649831",
                    " 15.8350169,43 25.5,43 L25.5,43"
                )
            )
        ),
        tag(
            `_tag_name` = "polygon",
            list(
                "class" = "icon-restart-symbol",
                "fill" = icon_color,
                "transform" = paste0(
                    "translate(39.500000, 33.722222)",
                    " rotate(-139.000000)",
                    " translate(-39.500000, -33.722222)"
                ),
                "points" = "39.5 29 48 38.4444444 31 38.4444444"
            )
        )
    )

    # return
    return(svg)
}

#' \code{warning}
#' @return create an svg warning icon
#' @name icons$warning
#' @usage icons$warning()
#' @param id a unique ID to be applied to the svg element
#' @param class a css class to be applied to the svg element
#' @param background_fill a color to be applied to the inner svg element
#' @param icon_color a color to be applied to the inner svg element
#' @param data_group a custom data attribute useful for subgroups of icons
#' @param aria_hidden a logical value for hiding elements
#'      (Useful for purely aesthetic elements)
#' @keywords icons warning
#' @importFrom htmltools tag tagList
#' @export
icons$warning <- function(
    id = NULL,
    class = NULL,
    background_fill = "#f6f6f6",
    icon_color = "#3f454b",
    data_group = NULL,
    aria_hidden = TRUE
) {

    # validate
    stopifnot(is.logical(aria_hidden))

    # svg parent
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "class" = "accessibleshiny-icon icon-warning",
            "width" = "50",
            "height" = "50",
            "viewBox" = "0 0 50 50",
            "aria-hidden" = tolower(as.character(aria_hidden)),
            "version" = "1.1",
            "xmlns" = "http://www.w3.org/2000/svg",
            "xmlns:xlink" = "http://www.w3.org/1999/xlink"
        )
    )

    # append id, class, data_group
    if (!is.null(id)) svg$attribs$id <- id
    if (!is.null(data_group)) svg$attribs$`data-group` <- data_group
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }

    # bind children: title, backround, symbol
    svg$children <- tagList(
        tag(
            `_tag_name` = "path",
            list(
                "stroke" = "none",
                "fill" = background_fill,
                "class" = "icon-warning-background",
                "d" = paste0(
                    "M26.6061578,12.1646762 L45.131772,37.1323078",
                    " C45.7899548,38.0193643 45.6044161,39.2720282",
                    " 44.7173596,39.930211 C44.3726637,40.1859703",
                    " 43.9548322,40.3240532 43.5256142,40.3240532",
                    " L6.47438579,40.3240532 C5.36981629,40.3240532",
                    " 4.47438579,39.4286227 4.47438579,38.3240532",
                    " C4.47438579,37.8948353 4.61246872,37.4770037",
                    " 4.86822798,37.1323078 L23.3938422,12.1646762",
                    " C24.052025,11.2776198 25.3046889,11.092081",
                    " 26.1917454,11.7502638 C26.3494827,11.8673026",
                    " 26.489119,12.0069389 26.6061578,12.1646762 Z"
                )
            )
        ),
        tag(
            `_tag_name` = "circle",
            list(
                "class" = "icon-warning-symbol icon-warning-dot",
                "fill" = icon_color,
                "cx" = "25",
                "cy" = "35",
                "r" = "2"
            )
        ),
        tag(
            `_tag_name` = "line",
            list(
                "stroke" = icon_color,
                "stroke-width" = "3",
                "stroke-linecap" = "round",
                "class" = "icon-warning-symbol icon-warning-line",
                "x1" = "25",
                "x2" = "25",
                "y1" = "19.875",
                "y2" = "29.125"
            )
        )
    )

    # return
    return(svg)
}