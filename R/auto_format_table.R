#' Automatically formats the values in each column according to their data type
#'
#' @param gt_table The static table
#'
#' @return A value formatted table
#'
#' @examples auto_format_table(summary_table)
auto_format_table <- function(gt_table) {
  gt_table <- gt_table |>
    fmt_date(
      columns = where(lubridate::is.Date),
      date_style = "m_day_year"
    ) |>
    fmt_number(
      columns = where(is.numeric),
      decimals = 2
    )

  return(gt_table)
}
