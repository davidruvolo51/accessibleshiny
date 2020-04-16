#' SVG Icons
#'
#' A set of svg icons
#' @name icons
#' @return A set of SVG icons
#' @keywords icons
#' @export
icons <- list()


#' \code{plus}
#' A plus sign with a filled background
#' @name icons$plus
#' @usage icons$plus()
#' @return A plus sign with a filled background
#' @param id a unique ID for the icon
#' @param class a css classname to assign to the icon
#' @param background_fill a background color for the icon (default: #3F454B)
#' @param icon_color a color for the plus symbol (default: #ffffff)
#' @keywords icons, plus
#' @export
icons$plus <- function(id = NULL, class = NULL, background_fill = "#3F454B", icon_color = "#ffffff") {

    # build svg element
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "aria-hidden" = "true",
            "class" = "icon",
            "width" = "50",
            "height" = "50"
        )
    )

    # build circle
    circle <- tag(
       `_tag_name` = "circle",
       list(
           "fill" = background_fill,
           "cx" = "25",
           "cy" = "25",
           "r" = "24"
       )
    )

    # build path
    path <- tag(
        `_tag_name` = "path",
        list(
            "d" = paste(
                "M25,15 C26.1045695,15 27,15.8954305 27,17",
                "L27,22.999 L33,23 C34.1045695,23 35,23.8954305",
                "35,25 C35,26.1045695 34.1045695,27 33,27 L27,27",
                "L27,33 C27,34.1045695 26.1045695,35 25,35",
                "C23.8954305,35 23,34.1045695 23,33 L23,27",
                "L17,27 C15.8954305,27 15,26.1045695 15,25",
                "C15,23.8954305 15.8954305,23 17,23 L23,23",
                "L23,17 C23,15.8954305 23.8954305,15",
                "25,15 Z"
            ),
            "fill" = icon_color
        )
    )

    # process svg attributes
    if (length(id) > 0) svg$attribs$id <- id
    if (length(class) > 0) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }

    # return
    svg$children <- tagList(circle, path)
    return(svg)
}