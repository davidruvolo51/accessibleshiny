

accordion <- function(title, html, ...) {
    stopifnot(!is.null(title))
    stopifnot(!is.null(html))

    # validate props
    props <- accordion_helpers$validate_props(...)

    # build element
    el <- tagList(
        accordion_helpers$heading(title, props = props),
        accordion_helpers$content(html, prop = props)
    )

    # return
    return(el)
}
