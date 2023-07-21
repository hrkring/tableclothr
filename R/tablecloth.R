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
#' @examples tablecloth(sample_data, title = 'My Sample Data', format = 'static', theme = 'greyscale')
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

  # Row Renaming
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

#' Constructs the dynamic table with an optional theme using DT
#'
#' @param data The table data
#' @param title The title of the table
#' @param theme The custom theme
#'
#' @return The formatted dynamic table
#'
#' @examples dynamic_table(financial_data, title = '2023 Income Statement', theme = 'greenscale')
dynamic_table <- function(data, rowname_col, group_col, title, theme) {

  # Row Names
  if (!identical(rowname_col, NULL)) {
    rownames <- as.character(data[[rowname_col]])
  } else {
    rownames <- ""
  }

  # Create Table
  dt_table <- data |>
    datatable(
      rownames = rownames,
      caption = htmltools::tags$caption(
        style = "font-family: verdana; caption-side: top; text-align: center; font-weight: bold; font-size: 25px; font-style: normal;",
        htmltools::em(title)
      ),
      extensions = c("Buttons", 'RowGroup'),
      options = list(
        order = list(
          list(which(names(data) == group_col), 'asc'),
          list(which(names(data) == rowname_col), 'asc')
        ),
        rowGroup = list(dataSrc = which(names(data) == group_col)),
        dom = "Bfrtip",
        buttons = c("csv", "excel", "pdf"),
        deferRender = TRUE,
        scrollY = 350,
        columnDefs = list(
          list(className = "dt-center", targets = "_all"),
          list(visible=FALSE,
               targets=c(which(names(data) == group_col),
                         which(names(data) == rowname_col)))
      )),
      class = "row-border hover compact stripe",
      fillContainer = TRUE
    )

  # Custom Theme
  if (!identical(theme, "default")) {
    dt_table <- dt_theme(dt_table, theme = theme)
  }

  return(dt_table)
}
