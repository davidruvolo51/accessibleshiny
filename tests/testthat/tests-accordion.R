#'////////////////////////////////////////////////////////////////////////////
#' FILE: tests-accordion.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-04-20
#' MODIFIED: 2020-09-10
#' PURPOSE: unit testing for accordion() function
#' STATUS: working; on.going
#' PACKAGES: accessibleshiny; testthat; htmltools; stringr
#' COMMENTS: NA
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
test_that("Confirm output is an HTML element", {
    expect_identical(
        object = class(
            accordion(
                inputId = "hello-world-def",
                title = "What does hello world mean?",
                content = tags$p("Hello world is a generic message")
            )
        ),
        expected = "shiny.tag",
        label = "Accordion element is not an html element"
    )
})



#' ~ b ~
#' accordion: elements
#' The returned output has the required elements (h3, section)
test_that("Assessment of basic structure", {

    # build component
    a <- accordion(
        inputId = "hello-world-def",
        title = "What does hello world mean?",
        content = tags$p("Hello world is a generic message")
    )

    # test
    expect_identical(
        object = c(a$children[[1]]$name, a$children[[2]]$name),
        expected = c("h3", "section"),
        label = "Accordion does not have the correct html elements"
    )
})


#' ~ c ~
#' Confirm `hidden` and `aria-expanded` attributes
test_that("Confirmation of `hidden` and `aria-expanded`", {
    a <- accordion(
        inputId = "hello-world-def",
        title = "What does hello world mean?",
        content = tags$p("Hello world is a generic message")
    )

    attrib_score <- 0

    if (length(a$children[[1]]$children[[1]]$attribs$`aria-expanded`)) {
        attrib_score <- attrib_score + 1
    }

    if (length(a$children[[2]]$attribs$hidden)) {
        attrib_score <- attrib_score + 1
    }

    expect_equal(
        object = attrib_score,
        expected = 2,
        label = paste0(
            "Attributes `hidden` and/or `aria-expanded` are not properly ",
            "added."
        )
    )
})


#' ~ d ~
#' Confirm that the value for `aria-controls` matches the ID of the section
test_that("Confirmation of `aria-controls`", {
    a <- accordion(
        inputId = "hello-world-def",
        title = "What does hello world mean?",
        content = tags$p("Hello world is a generic message")
    )

    expect_equal(
        object = (
            a$children[[1]]$children[[1]]$attribs$`aria-controls` ==
            a$children[[2]]$attribs$id
        ),
        expected = TRUE,
        label = paste0(
            "Value for `aria-controls` does not match the value ",
            "of section ID. This means that the elements are not ",
            "properly linked."
        )
    )
})

#'//////////////////////////////////////

#' ~ 2 ~
#' Evaluate Optional Arguments

#' Make sure the returned output element(s) have a unique ID
#' It is important to evaluate the unique ID that is generated as
#' it is used in the JS function that opens/close
test_that("Validation of grouping IDs", {

    # build component
    a <- accordion(
        inputId = "hello-world-def",
        title = "What does 'hello world' mean?",
        content = tags$p("Hello world is a generic message"),
    )

    # select elements that should have the grouping ID
    # these elements should be: h4, button, svg, and section
    # expected count (see test) should be 4
    count <- 0

    # find attribute for header (parent level first)
    h <- a$children[[1]]
    if (length(h$attribs$`data-accordion-group`) > 0) {
        if (h$attribs$`data-accordion-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # Does <section data-group=''> value match expected?
    section <- a$children[[2]]
    if (length(section$attribs$`data-accordion-group`) > 0) {
        if (section$attribs$`data-accordion-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # Does the <button> element have the same grouping ID?
    btn <- a$children[[1]]$children[[1]]
    if (length(btn$attribs$`data-accordion-group`) > 0) {
        if (btn$attribs$`data-accordion-group` == "hello-world-def") {
            count <- count + 1
        }
    }

    # test
    expect_equal(
        object = count,
        expected = 3,
        label = paste0(
            "Custom data attribute `data-accordion-group` is not properly",
            " added to child elements."
        )
    )
})

#'//////////////////////////////////////

#' ~ 3 ~
#' Evaluate Optional Style Arguments

#' ~ a ~
#' Evaluate Style Argument
#' The accordion component has optional style arguments that allow users to
#' modify the apperance of the accordion. Styles included are `flat` and
#' `focused`.
test_that("Validation of `style` argument", {

    # build components
    styles <- c("flat", "focused")
    sty_score <- 0
    sapply(seq_len(length(styles)), function(index) {
        elem <- accordion(
            inputId = "hello-world-def",
            title = "What does 'hello world' mean?",
            content = tags$p("Hello world is a generic message"),
            style = styles[index]
        )

        if (stringr::str_detect(elem$attribs$class, styles[index])) {
            sty_score <<- sty_score + 1
        }
    })

    # test
    expect_equal(
        object = sty_score,
        expected = length(styles),
        label = "Styles classnames are now properly added"
    )
})

#' ~ b ~
#' Evaluate Classnames argument
#'
#' The `classnames` argument can be used for passing user-defined css
#' classnames to the parent element of the accordion element (i.e., div)
#'
test_that("Validation of `classnames`", {
    expect_identical(
        object = accordion(
            inputId = "hello-world-def",
            title = "What does 'hello world' mean?",
            content = tags$p("Hello world is a generic message"),
            classnames = "dark-theme my-custom-classname"
        )$attribs$class,
        expected = "accordion accordion__flat dark-theme my-custom-classname",
        label = "`classnames` input is not added to parent element"
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
test_that("Validation of `heading_level`", {

    # headings
    h <- c("h1", "h2", "h3", "h4", "h5", "h6")
    h_score <- 0
    sapply(seq_len(length(h)), function(index) {

        # build temp element
        elem <- accordion(
            inputId = "hello-world-def",
            title = "What does 'hello world' mean?",
            content = tags$p("Hello world is a generic message"),
            heading_level = h[index]
        )

        # eval markup
        if (elem$children[[1]]$name == h[index]) {
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
