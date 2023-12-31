% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynamic_table.R
\name{dynamic_table}
\alias{dynamic_table}
\title{Construct a customized DT table}
\usage{
dynamic_table(
  data,
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
  theme = "default"
)
}
\arguments{
\item{data}{The input data.}

\item{title}{The table title.}

\item{subtitle}{The table subtitle.}

\item{rowname_col}{The rowname column.}

\item{group_col}{The grouping column.}

\item{autoformat}{Whether or not the values should be automatically formatted.}

\item{theme}{The custom theme.}
}
\value{
The formatted dynamic table
}
\description{
Construct a customized DT table
}
\examples{
dynamic_table(financial_data, title = '2023 Income Statement', theme = 'greenscale')
}
