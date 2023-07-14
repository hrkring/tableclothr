#' Title
#'
#' @param table
#' @param theme
#'
#' @return
#' @export
#'
#' @examples
gt_theme <- function(table, theme) {
  table <- table |>
    gt::tab_options(
      table.border.top.color = NULL,
      table.border.bottom.color = NULL,
      table.border.left.color = NULL,
      table.border.right.color = NULL,
      table_body.border.top.color = NULL,
      table_body.border.bottom.color = NULL,
      column_labels.background.color = NULL,
      column_labels.border.top.color = NULL,
      column_labels.border.bottom.color = NULL,
      column_labels.border.lr.color = NULL,
      row_group.border.top.color = NULL,
      row_group.border.bottom.color = NULL,
      row_group.border.left.color = NULL,
      row_group.border.right.color = NULL,
      column_labels.vlines.color = NULL
    )

  return(table)
}

#' Title
#'
#' @param table
#' @param theme
#'
#' @return
#' @export
#'
#' @examples
dt_theme <- function(table, theme) {

}
