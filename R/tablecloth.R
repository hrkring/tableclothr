#' This function takes a data frame as input and produces a stylized table
#' that can be static (gt) or dynamic (DT)
#'
#' @param data The data
#' @param title The title of your table
#' @param format The table format - 'gt' or 'dt'
#' @param theme The table theme
#'
#' @return A formatted table
#' @export
#'
#' @examples tablecloth(gtcars, format = 'dynamic', theme = 'greyscale')
tablecloth <- function(data,
                       table_number = NULL,
                       title = NULL,
                       subtitle = NULL,
                       rowname_col = NULL,
                       group_col = NULL,
                       aggregate_by = NULL,
                       autoformat = TRUE,
                       order_cols = NULL,
                       rename_cols = NULL,
                       hide_cols = NULL,
                       is_color_coded = FALSE,
                       theme = "default",
                       format = 'static') {
  if (format == "gt" | format == 'static' | format == 1) {
    tablecloth <- data |>
      static_table(
        title = title,
        subtitle = subtitle,
        rowname_col = rowname_col,
        group_col = group_col,
        aggregate_by = aggregate_by,
        theme = theme
      )
  } else if (format == "dt" | format == 'dynamic' | format == 2) {
    tablecloth <- data |>
      dynamic_table(
        title = title,
        rowname_col = rowname_col,
        group_col = group_col,
        theme = theme
      )
  } else {
    stop('Invalid table format')
  }
  return(tablecloth)
}
