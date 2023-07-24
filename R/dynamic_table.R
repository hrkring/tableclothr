#' Construct a customized DT table
#'
#' @param data The input data.
#' @param title The table title.
#' @param subtitle The table subtitle.
#' @param group_col The grouping column.
#' @param rowname_col The rowname column.
#' @param autoformat Whether or not the values should be automatically formatted.
#' @param theme The custom theme.
#'
#' @return The formatted dynamic table
#'
#' @examples dynamic_table(financial_data, title = '2023 Income Statement', theme = 'greenscale')
dynamic_table <- function(data,
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

  # Setting row names and grouping
  if (!identical(rowname_col, NULL) & !identical(group_col, NULL)) {
    rownames <- as.character(data[[rowname_col]])
    group <- list(dataSrc = which(names(data) == group_col))
    order <- list(list(which(names(data) == rowname_col), 'asc'), list(which(names(data) == group_col), 'asc'))
  }
  else if (!identical(rowname_col, NULL) & identical(group_col, NULL)) {
    rownames <- as.character(data[[rowname_col]])
    order <- list(list(which(names(data) == rowname_col), 'asc'))
  }
  else if (identical(rowname_col, NULL) & !identical(group_col, NULL)) {
    group <- list(dataSrc = which(names(data) == group_col))
    order <- list(list(which(names(data) == group_col), 'asc'))
  } else {
    rownames <- ""
    group <- NULL
    order <- NULL
  }

  # Table Numbering
  if (!identical(table_number, NULL) & identical(title, NULL)) {
    title <- paste0("Table ", table_number)
  } else if (!identical(table_number, NULL) & !identical(title, NULL)) {
    title <- paste0("Table ", table_number, ": ", title)
  }

  # Column Renaming
  if (!identical(rename_cols, NULL)) {
    colnames <- setNames(names(data), rename_cols)
  } else {
    colnames <- names(data)
  }

  # Column Hiding
  cols_to_hide <- list(
    visible=FALSE,
    targets = which(names(data) %in% c(group_col, rowname_col, hide_cols))
  )

  # Color Coding


  # Create Table
  dt_table <- data |>
    datatable(
      rownames = rownames,
      colnames = colnames,
      caption = htmltools::tags$caption(
        style = "font-family: verdana; caption-side: top; text-align: left; font-weight: bold; font-size: 25px; font-style: normal;",
        htmltools::em(title)
      ),
      extensions = c("Buttons", 'RowGroup'),
      options = list(
        order = order,
        rowGroup = group,
        dom = "Bfrtip",
        buttons = c("csv", "excel", "pdf"),
        deferRender = TRUE,
        scrollY = 350,
        columnDefs = list(
          list(className = "dt-center", targets = "_all"),
          cols_to_hide
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
