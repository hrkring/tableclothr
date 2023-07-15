#' Title
#'
#' @param table
#' @param group_col
#' @param aggregate_by
#'
#' @return
#'
#' @examples
aggregate_data <- function(data, group_by, aggregate_by = "sum") {
  data <- data |>
    group_by(!!! rlang::syms(group_by)) |>
    dplyr::summarise(dplyr::across(where(is.numeric), !!! rlang::syms(aggregate_by), na.rm = TRUE),
                  .groups = "drop")

  return(data)
}
