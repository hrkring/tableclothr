#' Change the table grouping
#'
#' @param table Input table.
#' @param group_col The grouping column.
#'
#' @return The grouped table.
#'
#' @examples
#' group_table(gtcars_table, group_col = "mfr")
group_table <- function(table, group_col = NULL) {

  for(nm in rev(unique(table[[1]][[group_col]])))
    table <- table %>%
      tab_row_group(label = nm,
                    rows = which(table[[1]][[group_col]] == nm))

  table <- table %>%
    gt::cols_hide(group_col)

  return(table)
}
