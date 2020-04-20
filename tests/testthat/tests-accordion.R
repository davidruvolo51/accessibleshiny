#'////////////////////////////////////////////////////////////////////////////
#' FILE: tests-accordion.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-04-20
#' MODIFIED: 2020-04-20
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

#' ~ 1 ~
#' evaluate accordion component

#' ~ a ~
#' accordion: exists
#' make sure an html element is returned
test_that("Output is an html element", {
    a <- accordion(
        title = "What does hello world mean?",
        html = tags$p("Hello world is a generic message")
    )
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
    a <- accordion(
        title = "What does hello world mean?",
        html = tags$p("Hello world is a generic message")
    )
    actual <- c(a[[1]]$name, a[[2]]$name, a[[3]]$name)
    expected <- c("h4", "section", "script")
    expect_identical(
        object = actual,
        expected = expected,
        label = "Accordion does not have the correct html elements"
    )
})