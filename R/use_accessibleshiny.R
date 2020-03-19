#' load_dependencies
#'
#' When called, this function attaches the css files in the <head> of the html
#' document.
#' @return Load package dependencies
#' @examples
#' use_accessiblyshiny()
#' @export
use_accessibleshiny <- function() {
    htmlDependency(
        name = "datatable",
        version = "0.1.4",
        src = "assets",
        package = "accessibleshiny",
        stylesheet = "css/accessibleshiny.min.css",
        all_files = FALSE
    )
}