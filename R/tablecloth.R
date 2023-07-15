#' This function takes a data frame as input and produces a stylized table
#' that can be static or dynamic
#'
#' @param data The data
#' @param title The title of your table
#' @param format The table format - 'static' or 'dynamic'
#' @param theme The table theme
#' @param ...
#'
#' @return A formatted table
#' @export
#'
#' @examples gs_tablecloth(sample_data, title = 'My Sample Data', format = 'static', theme = 'greyscale')
tablecloth <- function(data,
                       title = NULL,
                       subtitle = NULL,
                       rowname_col = NULL,
                       group_col = NULL,
                       aggregate_by = NULL,
                       format = 'static',
                       theme = 'default', ...) {
  if (format == 'static' | format == 1) {
    tablecloth <- data |> static_table(title = title,
                                       subtitle = subtitle,
                                       rowname_col = rowname_col,
                                       group_col = group_col,
                                       aggregate_by = aggregate_by,
                                       theme = theme)
  } else if (format == 'dynamic' | format == 2) {
    tablecloth <- data |> dynamic_table(title = title, theme = theme)
  } else {
    stop('Invalid table format')
  }
  return(tablecloth)
}

#' Constructs the static table with an optional theme using gt
#'
#' @param data The table data
#' @param title The title for the table
#' @param subtitle The subtitle for the table
#' @param group_col The grouping column
#' @param rowname_col The row name column
#' @param autoformat Whether or not the values should be automatically formatted
#' @param theme The custom theme
#'
#' @return The formatted static table
#'
#' @examples static_table(financial_data, theme = 'greenscale')
static_table <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         rowname_col = NULL,
                         group_col = NULL,
                         aggregate_by = NULL,
                         autoformat = TRUE,
                         theme) {

  # Table Aggregation
  if (!identical(group_col, NULL) & !identical(aggregate_by, NULL)) {
    data <- data |> aggregate_data(c(group_col, rowname_col), aggregate_by)
  }

  # Set Row IDs
  if (!identical(rowname_col, NULL)) {
    gt_table <- data |> gt(rowname_col = rowname_col)
  } else {
    gt_table <- data |> gt()
  }

  # Row Grouping
  if(!identical(group_col, NULL)) {
    gt_table <- gt_table |> group_table(group_col = group_col)
  }

  # Table Header Text
  if (identical(title, NULL) & !identical(subtitle, NULL)) {
    stop("Table cannot have only a subtitle")
  } else {
    gt_table <- gt_table |>
      tab_header(title = title, subtitle = subtitle)
  }

  if (identical(autoformat, TRUE)) {
    gt_table <- gt_table |> auto_format_table()
  }

  if (!identical(theme, "default")) {
    gt_table <- gt_table |> gt_theme(theme = theme)
  }

  return(gt_table)
}

#' Constructs the dynamic table with an optional theme using DT
#'
#' @param data The table data
#' @param title The title of the table
#' @param theme The custom theme
#'
#' @return The formatted dynamic table
#'
#' @examples dynamic_table(financial_data, title = '2023 Income Statement', theme = 'greenscale')
dynamic_table <- function(data, title, theme) {
  dt_table <- data |> dt_theme(title = title, theme = theme)

  return(dt_table)
}
