#'////////////////////////////////////////////////////////////////////////////
#' FILE: tests-datatable.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-01-28
#' MODIFIED: 2020-01-28
#' PURPOSE: unit testing for datatable() function
#' STATUS: working; on.going
#' PACKAGES: accessibleshiny; testthat
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////
context("datatable")

# ~ 1 ~
# evaluate table component

# ~ a ~
#' table : exists
# evaluate the opening <table> element
test_that("Returned Element is an html table", {
    tbl <- datatable(data = iris[1:2, ])
    elem <- as.character(tbl[[2]]$name)
    expect_identical("table", elem)
})

# ~ b ~
#' table : class attribute
# evaluate default classnames applied to the table element
test_that("Output has default css classes", {
    ref_css <- "datatable row-highlighting test"
    tbl <- datatable(data = iris[1:2, ], css = "test")
    elem_css <- as.character(tbl[[2]]$attribs$class)
    expect_equal(ref_css, elem_css)
})

# ~ c ~
# table : id attribute
# table id is present
test_that("Table is rendered with a unique id", {
    tbl <- datatable(data = iris[1:2, ], id = "test")
    tbl_id <- tbl[[2]]$attribs$id
    expect_identical("test", tbl_id)
})

# ~ d ~
#' table > thead + tbody
#' table has header (thead) and body (tbody) element
test_that("Table returns header and body", {
    tbl <- datatable(data = iris[1:2, ])
    tbl_elems <- length(tbl[[2]]$children)
    expect_equal(2, tbl_elems)
})

#'////////////////////////////////////////

# ~ 2 ~
# test table header element

# ~ a ~
#' table > thead > tr : row attribute
#' In the table header element (<thead>), it is important
#' to make sure that the row element (<tr>) has the attribute
#' role="row" defined. Since there is only one row in the header
#' we can extract the first element van the header
test_that("Role for table header row(s) has been defined", {
    tbl <- datatable(data = iris[1:2, ])
    tbl_role <- as.character(tbl[[2]]$children[[1]]$children[[1]]$attribs)
    expect_identical("row", tbl_role)
})

# ~ b ~
#' table > thead > tr > th : scope attribute
#' All table header cells have scope defined. The number of table
#' headers with scope should be identical to the number of
#' columns of the input dataset. To do this, extract the scope
#' attribute from each header cell and evaluate the number of cols
#' against the number of cells with scope. These values should match.
#' This is important responsive tables will modify the display properties
#' and we need to make sure the cells are linked to the headers.
test_that("All table headers have scope defined", {
    df <- iris[1:2, ]
    tbl <- datatable(data = df)
    thead <- tbl[[2]]$children[[1]]$children[[1]]$children[[1]]
    scopes <- list()
    lapply(seq_len(length(thead)), function(x) {
        scopes[[x]] <<- thead[[x]]$attribs
    })
    expect_equal(NCOL(df), length(scopes))
})

#'////////////////////////////////////////

# ~ 3 ~
# test table body element

# ~ a ~
#' table > tbody > tr : role attribute
#' Evaluate the presence of roles. For responsive table layouts, it is
#' important that each row in <tbody> has the role ="row" assigned. In this
#' test, the number of <tr> with the attribute "row" should match the
#' number of rows in of the input dataset.
test_that("All table body rows have role defined", {
    df <- iris[1:2, ]
    tbl <- datatable(data = df)
    tr <- tbl[[2]]$children[[2]]$children[[1]]
    roles <- list()
    lapply(seq_len(length(tr)), function(x) {
        roles[[x]] <<- tr[[x]]$attribs
    })
    expect_equal(NROW(df), length(roles))
})

# ~ b ~
#' table > tbody > tr > span : exists when responsive = TRUE
#' If the option responsive is TRUE (default), this function returns the markup
#' for an html table. This will render a <span> element inside each table
#' cell. In the css file, the span element will be visually hidden and
#' displayed at a specific breakpoint. This element servers no importance
#' to screen readers as the table header is still present and not hidden
#' from assistive web devices. This test will confirm that the <span> element
#' is present for all rows and cells.
test_that("Confirm markup for responsive tables", {
    df <- iris[1:2, ]
    tbl <- datatable(data = df, options = list(responsive = TRUE))
    tr <- tbl[[2]]$children[[2]]$children[[1]]
    spans <- c()
    lapply(seq_len(length(tr)), function(row) {
        cells <- tr[[row]]$children[[1]]
        spans_in_curr_row <- list()
        lapply(seq_len(length(cells)), function(col) {
            spans_in_curr_row[[col]] <<- cells[[col]]$children[[1]]$name
        })
        spans[[row]] <<- length(spans_in_curr_row)
    })
    expected_html_cells <- NROW(df) * NCOL(df)
    actual_html_cells <- sum(spans)
    expect_equal(expected_html_cells, actual_html_cells)
})

# ~ c ~
#' table > tbody > tr > span : !exist when responsive = FALSE
#' If the option "responsive" is FALSE, then the function will return the
#' html markup without a <span> element. This function will work in a similar
#' manner as the previous unit test. Using the input dataset, it will extract
#' table rows from the <tbody> element. Then using nested lapply's, it will
#' evaluate the peresence of child elements in each cell. It is expected that
#' there will be one element as this is the cell value itself. However, we are
#' looking for more elements. In this case, the test will return the number
#' if child elements in each cell and then sum them rowwise. If the total
#' number is greater than 0, then the test will fail as this indicates that
#' there are child elements present.
test_that("Inline elements are not rendered when responsive = FALSE", {
    df <- iris[1:2, ]
    tbl <- datatable(data = df, options = list(responsive = FALSE))
    tr <- tbl[[2]]$children[[2]]$children[[1]]
    spans <- c()
    lapply(seq_len(length(tr)), function(row) {
        cells <- tr[[row]]$children[[1]]
        spans_in_curr_row <- c()
        lapply(seq_len(length(cells)), function(col) {
            cell_children <- cells[[col]]$children
            if (length(cell_children) %in% c(0, 1)) {
                spans_in_curr_row[[col]] <<- 0
            } else {
                spans_in_curr_row[[col]] <<- 1
            }
        })
        spans[[row]] <<- sum(spans_in_curr_row)
    })
    expect_equal(0, sum(spans))
})