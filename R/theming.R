#" Provides custom theming functionality for gt tables
#"
#" @param table  The gt table to be themed
#" @param theme The theme to apply to the table
#"
#" @return The themed gt table
#" @export
#"
#" @examples gt_theme(financial_data, theme = "greenscale")
gt_theme <- function(table, theme = "greyscale") {
  if (theme %in% c(NA, NaN, NULL)) {
    stop("Invalid input.")
  }

  if (!theme %in% tableclothr:::themes$Theme.Name) {
    stop("That theme does not exist! Use available_themes() to see what themes you can use.")
  }

  themes <- dplyr::filter(tableclothr:::themes, Theme.Name == theme)

  # Main Theming Components
  table <- table |>
    tab_options(
      heading.background.color = themes$Heading.Background.Color,
      heading.border.bottom.color = themes$Heading.Border.Bottom.Color,

      column_labels.background.color = themes$Column_Labels.Background.Color,
      column_labels.font.weight = themes$Column_Labels.Font.Weight,
      column_labels.border.top.color = themes$Column_Labels.Border.Top.Color,
      column_labels.border.bottom.color = themes$Column_Labels.Border.Bottom.Color,

      row_group.background.color = themes$Row_Group.Background.Color,
      row_group.font.weight = themes$Row_Group.Font.Weight,
      row_group.border.top.color = themes$Row_Group.Border.Top.Color,
      row_group.border.bottom.color = themes$Row_Group.Border.Bottom.Color,
      row_group.padding = themes$Row_Group.Padding,

      row.striping.background_color = themes$Row.Striping.Background_Color,
      row.striping.include_table_body = themes$Row.Striping.Include_Table_Body,

      summary_row.background.color = themes$Summary_Row.Background.Color,
      summary_row.border.color = themes$Summary_Row.Border.Color,

      grand_summary_row.background.color = themes$Grand_Summary_Row.Background.Color,
      grand_summary_row.border.color = themes$Grand_Summary_Row.Border.Color,

      table.border.top.color = themes$Table.Border.Top.Color,
      table.border.bottom.color = themes$Table.Border.Bottom.Color,
      table.border.left.color = themes$Table.Border.Left.Color,
      table.border.right.color = themes$Table.Border.Right.Color,
      table.border.left.style = themes$Table.Border.Left.Style,
      table.border.right.style = themes$Table.Border.Right.Style,

      table_body.border.top.color = themes$Table_Body.Border.Top.Color,
      table_body.border.bottom.color = themes$Table_Body.Border.Bottom.Color,

      table.font.color = themes$Table.Font.Color,
      table.font.size = themes$Table.Font.Size,

      footnotes.background.color = themes$Footnotes.Background.Color,

      source_notes.background.color = themes$Source_Notes.Background.Color,

      data_row.padding = themes$Data_Row.Padding,

      container.overflow.x = themes$Container.Overflow.x
    ) |>
    tab_style(
      style = cell_text(color = themes$Heading.Text.Color),
      locations = list(
        cells_title(groups = "title"),
        cells_column_labels(columns = everything())
      )
    ) |>
    tab_style(
      style = cell_text(color = themes$Heading.Text.Color),
      locations = cells_title(groups = "subtitle")
    ) |>
    opt_align_table_header(align = "left")

  return(table)
}

#" Provides custom theming functionality for DT tables
#"
#" @param table  The DT table to be themed
#" @param theme The theme to apply to the table
#"
#" @return The themed DT table
#" @export
#"
#" @examples dt_theme(financial_data, title = "2023 Financial Earnings", theme = "greenscale")
dt_theme <- function(table, theme = "greyscale") {
  if (theme %in% c(NA, NaN, NULL)) {
    stop("Invalid input.")
  }

  if (!theme %in% tableclothr:::themes$Theme.Name) {
    stop("That theme does not exist! Use available_themes() to see what themes you can use.")
  }

  themes <- dplyr::filter(tableclothr:::themes, Theme.Name == theme)

  # Main Theming Components
  initComplete <- JS(paste0(
    "function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '", themes$Heading.Background.Color, "',
        'color': '", themes$Heading.Text.Color, "'
        });
      $('button.buttons-csv').css('background-color','", themes$Heading.Background.Color, "');
      $('button.buttons-excel').css('background-color','", themes$Heading.Background.Color, "');
      $('button.buttons-pdf').css('background-color','", themes$Heading.Background.Color, "');
      $('button.buttons-csv').css('color','", themes$Heading.Text.Color, "');
      $('button.buttons-excel').css('color','", themes$Heading.Text.Color, "');
      $('button.buttons-pdf').css('color','", themes$Heading.Text.Color, "');
      return table;
      }"))

  # Set Row Striping
  rowCallback <- JS(paste0(
    "function(row, data, num, index){",
    "  var $row = $(row);",
    "  if($row.hasClass('even')){",
    "    $row.css('background-color', '", themes$Row.Striping.Background_Color, "');",
    "  }else{",
    "    $row.css('background-color', 'white');",
    "  }",
    "}"
  ))

  table[['x']][['options']][['rowCallback']] <- rowCallback
  table[['x']][['options']][['initComplete']] <- initComplete

  return(table)
}
