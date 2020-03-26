> The package is working, but I would wait before implementing into any production application before all underlying methods have been thoroughly tested.

# Accessible Shiny

The accessible shiny package contains a series of accessible ui components for use in shiny applications. This project is in the early days of development and more fun things are on the way! 

## Current Features

- [x] Responsive Datatable
- ...


Please refer to the wiki for more information about package development and function reference.

## Installation

You can install the `accessibleshiny` package using the `devtools` package.

```r
install.packages("devtools")
devtools::install_github("davidruvolo51/accessibleshiny")
```


## Example

Here's how to create a responsive datatable using this package.

```r
library(shiny)
library(tidyverse)

# source birds dataset from tidytuesday 2019-06-18
birds <- read.csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv"
)

# summarize and find the top 25 birds
bird_summary <- birds %>%
    group_by(species) %>%
    summarize(
        total_count = sum(how_many_counted, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        total = sum(total_count),
        rate = round(100 * (total_count / total), 2)
    ) %>%
    select(-total) %>%
    arrange(-total_count) %>%
    slice(1:25) %>%
    mutate(species = paste0(
        "<a href='http://www.google.com/search?q=",
        gsub(" ", "%20", species),
        "'>",
        species,
        "</a>"
    ))


# ui
ui <- tagList(
    accessibleshiny::use_accessibleshiny(),
    tags$head(
        tags$style(
            "html, body{
                padding: 0;
                margin: 0;
                font-family: Helvetica, sans-serif;
                font-size: 16pt;
            }",
            "main {
                width: 90%;
                margin: 0 auto;
            }",
            ".column-2, .column-3 {
                text-align: left;
            }",
            "@media (min-width: 912px) {",
                "main {
                    max-width: 912px;
                }",
                ".column-2, .column-3 {
                    text-align: center;
                }",
            "}"
        )
    ),
    tags$main(
        tags$h2("Responsive Datatable Example"),
        uiOutput("tbl")
    )
)


# server
server <- function(input, output) {
    output$tbl <- renderUI({
        accessibleshiny::datatable(
            data = bird_summary,
            caption = "Top 25 Most Reported Birds in the Christmas Bird Count since 1921",
            options = list(
                html_escape = FALSE
            )
        )
    })
}

# run app
shinyApp(ui, server)
```