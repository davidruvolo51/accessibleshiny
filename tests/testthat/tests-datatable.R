#'////////////////////////////////////////////////////////////////////////////
#' FILE: tests-datatable.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-01-28
#' MODIFIED: 2020-09-16
#' PURPOSE: unit testing for datatable() function
#' STATUS: working; on.going
#' PACKAGES: accessibleshiny; testthat
#' COMMENTS: Run from `dev/dev.R`
#'////////////////////////////////////////////////////////////////////////////

# pkgs
library(testthat)
library(accessibleshiny)


# create sample dataset
df <- data.frame(
    group = sample(LETTERS[1:3], 5, replace = TRUE),
    xvar = rnorm(5, mean = 15, sd = 2),
    yvar = rnorm(5, mean = 35, sd = 2)
)

# ~ 1 ~
# evaluate table component

# ~ a ~
#' table : exists
# evaluate the opening <table> element
test_that("Returned Element is an html table", {
    expect_identical(
        object = datatable(data = df)$name,
        expected = "table",
        label = "Returned element is not an html table"
    )
})

# ~ b ~
#' table : class attribute
# evaluate default classnames applied to the table element
test_that("Output has default css classes", {
    expect_equal(
        object = strsplit(
            x = datatable(data = df, classnames = "test")$attribs$class,
            split = " "
        )[[1]],
        expected = c(
            "datatable",
            "datatable__responsive",
            "row__highlighting",
            "test"
        ),
        label = "Table does not have expected css classes"
    )
})

# ~ c ~
# table : id attribute
# table id is present
test_that("Table is rendered with a unique ID", {
    expect_identical(
        object = datatable(data = df, id = "myTable")$attribs$id,
        expected = "myTable",
        label = "Table is not rendered with the ID attribute"
    )
})

# ~ d ~
#' table > thead + tbody
#' table has header (thead) and body (tbody) element
test_that("Table returns header and body", {
    tbl <- datatable(data = df)$children
    expect_equal(
        object = c(
            tbl[[1]]$name,
            tbl[[2]]$name
        ),
        expected = c("thead", "tbody"),
        label = "Table is not rendered with basic elements: head and body"
    )
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
    tbl <- datatable(data = df)
    tbl_role <- as.character(tbl$children[[1]]$children[[1]]$attribs$role)
    expect_identical(
        object = tbl_role,
        expected = "row",
        label = "Role attributes in table header have not been defined"
    )
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
    tbl <- datatable(data = df)
    thead <- tbl$children[[1]]$children[[1]]$children[[1]]
    scopes <- list()
    lapply(seq_len(length(thead)), function(x) {
        scopes[[x]] <<- thead[[x]]$attribs
    })
    expect_equal(
        object = length(scopes),
        expected = NCOL(df),
        label = "All table headers do not have scopes defined"
    )
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
    tbl <- datatable(data = df)
    tr <- tbl$children[[2]]$children[[1]]
    roles <- list()
    lapply(seq_len(length(tr)), function(x) {
        roles[[x]] <<- tr[[x]]$attribs
    })
    expect_equal(
        object = length(roles),
        expected = NROW(df),
        label = "All Table rows do not have a role defined"
    )
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
    tbl <- datatable(data = df, is_responsive = TRUE)
    tr <- tbl$children[[2]]$children[[1]]
    spans <- c()
    lapply(seq_len(length(tr)), function(row) {
        cells <- tr[[row]]$children[[1]][[1]]
        spans_in_curr_row <- list()
        lapply(seq_len(length(cells)), function(col) {
            spans_in_curr_row[[col]] <<- cells[[col]]$children[[1]]$name
        })
        spans[[row]] <<- length(spans_in_curr_row)
    })
    expected_html_cells <- NROW(df) * NCOL(df)
    actual_html_cells <- sum(as.numeric(spans))
    expect_equal(
        object = actual_html_cells,
        expected = expected_html_cells,
        label = "Responsive markup is not properly generated"
    )
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
    tbl <- datatable(data = df, is_responsive = FALSE)
    tr <- tbl$children[[2]]$children[[1]]
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
        spans[[row]] <<- sum(as.numeric(spans_in_curr_row))
    })
    expect_equal(
        object = sum(as.numeric(spans)),
        expected = 0,
        label = "Inline elements were generated despite the FALSE setting"
    )
})


#'//////////////////////////////////////

#' ~ 4 ~
#' Evaluate cell level markup


#' ~ a ~
#' Do cells have the attribute `role = "gridcell"`?
#' In this test, look make sure all cells have the attribute
#' `role` and the value is "gridcell". Make sure the lengths
#' of the attributes and cells match
test_that("Table body cells have role attribute defined as 'gridcell'", {
    tbl <- datatable(data = df)
    tbody <- tbl$children[[2]]
    cells <- tbody$children[[1]][[1]]$children[[1]][[1]]
    roles <- 0
    lapply(seq_len(length(cells)), function(cell) {
        html <- cells[[cell]]
        if (html$attribs$role == "gridcell") {
            roles <<- roles + 1
        }
    })
    expect_equal(
        object = roles,
        expected = NCOL(df),
        label = "Table cells do not have a role attribute"
    )
})


#'//////////////////////////////////////

#' ~ 5 ~
#' test caption
#' Using the input argument `caption` and option `caption_below`,
#' evaluate the rendered caption. If the input argument is TRUE,
#' the caption should be positioned below the table. However,
#' the positioning is handled by adding or removing as css class
#' that modifies the `caption-side` property. If the argument is
#' TRUE, then the class `caption-side-bottom` is added to the
#' <table> element. Therefore, this test will evaluate the
#' returned css classes.

#' ~ a ~
#' Make sure the caption will be positioned before the table (default)
test_that("Caption is positioned before the table", {
    expect_equal(
        object = strsplit(
            x = datatable(
                data = df,
                caption = "Test Caption",
                caption_placement = "top"
            )$attribs$class,
            split = " "
        )[[1]],
        expected = c(
            "datatable",
            "datatable__responsive",
            "caption__side__top",
            "row__highlighting"
        ),
        label = "Caption is not positioned before the table"
    )
})

#' ~ b ~
#' Make sure the caption will be positioned after the table
test_that("Caption is positioned after the table", {
    expect_equal(
        object = strsplit(
            x = datatable(
                data = df,
                caption = "Test Caption",
                caption_placement = "bottom"
            )$attribs$class,
            split = " "
        )[[1]],
        expected = c(
            "datatable",
            "datatable__responsive",
            "caption__side__bottom",
            "row__highlighting"
        ),
        label = "Caption is not positioned before the table"
    )
})