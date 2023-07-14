#' Title
#'
#' @param data
#' @param format
#' @param theme
#'
#' @return The formatted table
#' @export
#'
#' @examples
tablecloth <- function(data, format, theme) {
  if (format == "static" | format == 1) {
    gt_table(data, theme)
  } else if (format == "dynamic" | format == 1) {
    dt_table(data, theme)
  } else {
    stop("Invalid format.")
  }
}

#' Title
#'
#' @param data
#' @param theme
#'
#' @return The formatted gt table
#'
#' @examples
gt_table <- function(data, theme = "default") {
  table <- data |>
    gt::gt()

  return(table)
}

#' Title
#'
#' @param data
#' @param theme
#'
#' @return The formatted DT table
#'
#' @examples
dt_table <- function(data, theme = "default") {
  table <- data |>
    DT::datatable()

  return(table)
}
