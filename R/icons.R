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
#' @param background_fill a background color for the icon (default: #f6f6f6)
#' @param icon_color a color for the plus symbol (default: #3f454b)
#' @param data_group a custom data attribute for use in the accordion function
#' @keywords icons, plus
#' @importFrom htmltools tag tagList
#' @export
icons$plus <- function(id = NULL, class = NULL, background_fill = "#f6f6f6", icon_color = "#3f454b", data_group = NULL) {

    # build svg element
    svg <- tag(
        `_tag_name` = "svg",
        list(
            "aria-hidden" = "true",
            "class" = "icon icon-plus",
            "width" = "25",
            "height" = "25",
            "viewBox" = "0 0 25 25"
        )
    )

    # build plus sign using two svg line elements
    svg$children <- tagList(

        # circle for filled background
        tag(
            `_tag_name` = "circle",
            list(
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
                "x1" = "12.5",
                "y1" = "5",
                "x2" = "12.5",
                "y2" = "20",
                "stroke" = icon_color,
                "stroke-linecap" = "butt",
                "stroke-width" = "2.5"
            )
        ),

        # horizontal lines
        tag(
            `_tag_name` = "line",
            list(
                "x1" = "5",
                "y1" = "12.5",
                "x2" = "20",
                "y2" = "12.5",
                "stroke" = icon_color,
                "stroke-linecap" = "butt",
                "stroke-width" = "2.5"
            )
        )
    )

    # process attributes
    if (length(id) > 0) svg$attribs$id <- id
    if (length(data_group) > 0) svg$attribs$`data-group` <- data_group
    if (length(class) > 0) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }

    # return
    return(svg)
}