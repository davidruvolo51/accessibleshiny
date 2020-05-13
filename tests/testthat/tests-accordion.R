#'////////////////////////////////////////////////////////////////////////////
#' FILE: tests-accordion.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-04-20
#' MODIFIED: 2020-05-13
#' PURPOSE: unit testing for accordion() function
#' STATUS: working; on.going
#' PACKAGES: accessibleshiny; testthat; htmltools
#' COMMENTS: Run from parent dir using `npm run test`
#'////////////////////////////////////////////////////////////////////////////
options(stringsAsFactors = FALSE)

# pkgs
library(testthat)
library(accessibleshiny)
library(htmltools)
library(stringr)

#' ~ 1 ~
#' evaluate accordion component

#' ~ a ~
#' accordion: exists
#' make sure an html element is returned
test_that("Output is an html element", {

    # build component
    a <- accordion(
        title = "What does hello world mean?",
        html = tags$p("Hello world is a generic message")
    )

    # test
    expect_identical(
        object = class(a)[1],
        expected = "shiny.tag.list",
        label = "Accordion element is not an html element"
    )
})



#' ~ b ~
#' accordion: elements
#' The returned output has the required elements (h4, section, script)
test_that("Output has correct elements", {

    # build component
    a <- accordion(
        title = "What does hello world mean?",
        html = tags$p("Hello world is a generic message")
    )

    # evaluate
    actual <- c(a[[1]]$name, a[[2]]$name, a[[3]]$name)
    expected <- c("h4", "section", "script")

    # test
    expect_identical(
        object = actual,
        expected = expected,
        label = "Accordion does not have the correct html elements"
    )
})

#'//////////////////////////////////////

#' ~ 2 ~
#' Evaluate Optional Arguments

#' Make sure the returned output element(s) have a unique ID
#' It is important to evaluate the unique ID that is generated as
#' it is used in the JS function that opens/close
test_that("Outputs have a unique grouping ID", {

    # build component
    a <- accordion(
        title = "What does 'hello world' mean?",
        html = tags$p("Hello world is a generic message"),
        id = "hello-world-def"
    )

    # select elements that should have the grouping ID
    # these elements should be: h4, button, svg, and section
    # expected count (see test) should be 4
    count <- 0

    # find attribute for header (parent level first)
    if (length(a[[1]]$attribs$`data-group`) > 0) {
        if (a[[1]]$attribs$`data-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # Does <section data-group=''> value match expected?
    if (length(a[[2]]$attribs$`data-group`) > 0) {
        if (a[[2]]$attribs$`data-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # Does the <button> element have the same grouping ID?
    btn <- a[[1]]$children
    if (length(btn$attribs$`data-group`) > 0) {
        if (btn$attribs$`data-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # Does the <svg> element have the same grouping ID
    svg <- btn$children[[2]]
    if (length(svg$attribs$`data-group`) > 0) {
        if (svg$attribs$`data-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # test
    expect_equal(
        object = count,
        expected = 4,
        label = "Outputs do not have a unique grouping ID"
    )
})

#'//////////////////////////////////////

#' ~ 3 ~
#' Evaluate Optional Style Arguments

#' ~ b ~
#' Evaluate Icon Background
#' The accordion component has optional style arguments that allow users to
#' modify the apperance of the accordion toggle on-the-fly. Evaluate the
#' optional argument: icon_background to ensure values are passed
#' down accordingly.
test_that("SVG Icon has user-defined background color", {

    # build component
    a <- accordion(
        title = "What does 'hello world' mean?",
        html = tags$p("Hello world is a generic message"),
        style = list(
            icon_background = "#bde4a7"
        )
    )

    # extract color attribute from <circle> element
    status <- 0
    circle <- a[[1]]$children$children[[2]]$children[[1]]
    if (length(circle$attribs$fill) > 0) {
        status <- status + 1
        if (circle$attribs$fill == "#bde4a7") {
            status <- status + 1
        }
    }

    # test
    expect_equal(
        object = status,
        expected = 2,
        label = "SVG icon does not have user-defined background color"
    )
})

#' ~ c ~
#' Evaluate Icon Fill Color
#' The accordion component has optional style arguments that allow users to
#' modify the apperance of the accordion toggle on-the-fly. Evaluate the
#' optional argument: icon_color to ensure values are passed down accordingly.
test_that("SVG Icon has user-defined fill color", {

    # build component
    a <- accordion(
        title = "What does 'hello world' mean?",
        html = tags$p("Hello world is a generic message"),
        style = list(
            icon_color = "#bde4a7"
        )
    )

    # extract color attribute from <path> element
    status <- 0
    path <- a[[1]]$children$children[[2]]$children[[2]]
    if (length(path$attribs$stroke) > 0) {
        status <- status + 1
        if (path$attribs$stroke == "#bde4a7") {
            status <- status + 1
        }
    }

    # test
    expect_equal(
        object = status,
        expected = 2,
        label = "SVG Icon does not user-defined fill color"
    )
})


#'//////////////////////////////////////

#' ~ 4 ~
#' Evaluate Optional Options Arguments
 

#' ~ a ~
#' Evaluate Heading Options
#' The returned heading element is <h4>. However, this element may not always
#' work with the user's document structure (i.e., hierarchy). The option
#' `heading_level` allows users to choose which heading element they want
#' (i.e., h1:6). This test ensures the function accepts proper html elements.
test_that("Heading elements are properly changed", {

    # headings
    h <- c("h1", "h2", "h3", "h4", "h5", "h6")
    h_score <- 0
    sapply(seq_len(length(h)), function(index) {

        # build temp element
        elem <- accordion(
            title = "What does 'hello world' mean?",
            html = tags$p("Hello world is a generic message"),
            options = list(
                heading_level = h[index]
            )
        )

        # eval markup
        if (elem[[1]]$name == h[index]) {
            h_score <<- h_score + 1
        }
    })

    # test
    expect_equal(
        object = h_score,
        expected = length(h),
        label = "Heading elements do not match user-defined heading levels"
    )
})


#' ~ b ~
#' Evaluate Starting Option
#' The argument `start_open` allows users to render the accordion component
#' open. However, it we need to make sure the all classes and attributes are
#' properly defined. This test evaluates the component and returns a score
#' for each correct modified property. The total must be: 4
test_that("Accordion rendered Open", {

    # build component
    a <- accordion(
        title = "What does 'hello world' mean?",
        html = tags$p("Hello world is a generic message"),
        options = list(
            start_open = TRUE
        )
    )

    # score
    score <- 0

    # Button should have `aria-expanded = 'true'`
    btn <- a[[1]]$children
    if (btn$attribs$`aria-expanded` == "true") score <- score + 1

    # SVG should have `class = "... rotated"`
    svg <- a[[1]]$children$children[[2]]
    svg_classes <- stringr::str_split(svg$attribs$class, " ")[[1]]
    if (svg_classes[length(svg_classes)] == "rotated") {
        score <- score + 1
    }

    # Section should have `class = '... accordion-hidden'`
    section <- a[[2]]
    section_classes <- stringr::str_split(section$attribs$class, " ")[[1]]
    if (!"accordion-hidden" %in% section_classes) {
        score <- score + 1
    }

    # Section should not have attribute `hidden`
    if (length(section$attribs$hidden) == 0) {
        score <- score + 1
    }

    # test
    expect_equal(
        object = score,
        expected = 4,
        label = "Accordion is not properly rendered as 'open'"
    )
})
