#' Construct a customized gt table.
#'
#' @param data The input data.
#' @param title The table title.
#' @param subtitle The table subtitle.
#' @param group_col The grouping column.
#' @param rowname_col The rowname column.
#' @param autoformat Whether or not the values should be automatically formatted.
#' @param theme The custom theme.
#'
#' @return The formatted static table
#'
#' @examples static_table(financial_data, theme = 'greenscale')
static_table <- function(data,
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
                         theme = "default") {

  # Table Aggregation
  if (!identical(group_col, NULL) & !identical(aggregate_by, NULL)) {
    data <- data |> aggregate_data(c(group_col, rowname_col), aggregate_by)
  }

  # Set Row Names, Create Table
  if (!identical(rowname_col, NULL)) {
    gt_table <- data |> gt(rowname_col = rowname_col)
  } else {
    gt_table <- data |> gt()
  }

  # Row Grouping
  if(!identical(group_col, NULL)) {
    gt_table <- gt_table |> group_table(group_col = group_col)
  }

  # Table Numbering
  if (!identical(table_number, NULL) & identical(title, NULL)) {
    title <- paste0("Table ", table_number)
  } else if (!identical(table_number, NULL) & !identical(title, NULL)) {
    title <- paste0("Table ", table_number, ": ", title)
  }

  # Table Header Text
  if (!identical(title, NULL) & !identical(subtitle, NULL)) {
    gt_table <- gt_table |>
      tab_header(title = title, subtitle = subtitle)
  } else if (identical(title, NULL) & !identical(subtitle, NULL)) {
    stop("Table cannot have only a subtitle")
  }  else if (!identical(title, NULL) & identical(subtitle, NULL)) {
    gt_table <- gt_table |>
      tab_header(title = title, subtitle = paste("Generated as of: ", format(Sys.Date(), format = "%B %d, %Y")))
  }

  # Format Table by Datatypes
  if (identical(autoformat, TRUE)) {
    gt_table <- gt_table |> auto_format_table()
  }

  # Column Ordering
  if (!identical(order_cols, NULL)) {
    gt_table <- gt_table |> cols_move_to_start(columns = order_cols)
  }

  # Column Hiding
  if (!identical(hide_cols, NULL)) {
    gt_table <- gt_table |> cols_hide(all_of(hide_cols))
  }

  # Column Renaming
  if (!identical(rename_cols, NULL)) {
    gt_table <- gt_table |> cols_label(.list = rename_cols)
  }

  # Color Coding
  if (is_color_coded) {
    gt_table <- gt_table |> add_color_code(table_data, gt_table)
  }

  # Custom Theme
  if (!identical(theme, "default")) {
    gt_table <- gt_table |> gt_theme(theme = theme)
  }

  return(gt_table)
}
