#' Aggregate data by one or more grouping columns
#'
#' @param data Input data.
#' @param group_cols The grouping column(s).
#' @param aggregate_by The aggregation method (sum, mean, median, etc.).
#'
#' @return The aggregated data.
#'
#' @examples
#' aggregate_data(gtcars, c("mfr", "year"), "mean")
aggregate_data <- function(data, group_cols, aggregate_by = "sum") {
  data <- data |>
    dplyr::group_by(!!! rlang::syms(group_cols)) |>
    dplyr::summarise(dplyr::across(where(is.numeric), !!! rlang::syms(aggregate_by), na.rm = TRUE),
                  .groups = "drop")

  return(data)
}
