#' Title
#'
#' @param table
#' @param group_col
#' @param aggregate_by
#'
#' @return
#'
#' @examples
aggregate_data <- function(data, group_cols, aggregate_by = "sum") {
  data <- data |>
    dplyr::group_by(!!! rlang::syms(group_cols)) |>
    dplyr::summarise(dplyr::across(where(is.numeric), !!! rlang::syms(aggregate_by), na.rm = TRUE),
                  .groups = "drop")

  return(data)
}
