#' Add 3 level color coding to a column in your table
#'
#' @param table A gt table.
#' @param col The column to be color coded.
#' @param thresholds The thresholds for the column being color coded.
#' @param colors The color scale to apply.
#'
#' @return a color coded gt table.
#' @export
#'
#' @examples
#' add_color_code(gtcars_table, "hp", c(500, 1500), c("red", "yellow", "green"))
add_color_code <- function(table, col, thresholds = NULL, colors = NULL) {
  # Use defaults if no value supplied
  if (identical(thresholds, NULL)) {
    thresholds <- unname(quantile(table[[1]][[col]], probs = c(.333, .667)))
  }
  if (identical(colors, NULL)) {
    colors <- c("palegreen3", "lightgoldenrodyellow", "darksalmon")
  }

  # Error is thresholds and colors are wrong lengths
  if(length(thresholds) != 2) {
    stop("You must specify a vector containing a lower and upper bound.")
  }
  if(length(colors) != 3) {
    stop("You must specify a vector containing 3 colors.")
  }

  # Add color reference column to table
  table[[1]] <- table[[1]] |>
    mutate(across(col, ~ case_when(
      . < thresholds[1] ~ colors[1],
      . >= thresholds[1] & . < thresholds[2] ~ colors[2],
      . >= thresholds[2] ~ colors[3]
    ),
    .names = "{.col}.color"
    ))

  color_cols <- table[[1]] |>
    select(tidyselect::contains(".color")) |>
    names()

  # Apply color codings
  for (color_col in color_cols) {
    target_col <- gsub(pattern = ".color", replacement = "", color_col)
    for (specific_color in unique(table[[1]][[color_col]])) {
      table <- table |>
        tab_style(
          style = cell_fill(color = case_when(
            specific_color == colors[1] ~ colors[1],
            specific_color == colors[2] ~ colors[2],
            specific_color == colors[3] ~ colors[3]
          )),
          locations = cells_body(
            columns = target_col,
            rows = .data[[{{ color_col }}]] == specific_color
          )
        )
    }
  }

  # Remove reference column
  table[[1]] <- table[[1]] |>
    select(-c(color_cols))

  return(table)
}

#' Add 3 level color coding to one or more columns in your table
#'
#' @param table A gt table.
#' @param cols The columns to be color coded.
#' @param thresh_list A list of thresholds for each column being color coded.
#' @param colors The color scale to apply.
#'
#' @return A color coded gt table.
#' @export
#'
#' @examples
#' add_color_code(gtcars_table, c("hp", "trq"), list(c(500, 1500), c(500, 1500)), c("red", "yellow", "green"))
add_color_codes <- function(table, cols, thresh_list = NULL, colors = NULL) {
  if (!identical(names(table), names(gt(mtcars)))) {
    stop("Table is not a gt table.")
  }

  if (length(cols) != length(thresh_list) & !identical(thresh_list, NULL)) {
    stop("The number of columns and thresholds must be the same.")
  }

  for(i in seq(length(cols))) {
    if (!identical(thresh_list, NULL)) {
      table <- table |> add_color_code(col = cols[i], thresholds = thresh_list[[i]], colors = colors)
    } else {
      table <- table |> add_color_code(col = cols[i], colors = colors)
    }
  }

  return(table)
}
