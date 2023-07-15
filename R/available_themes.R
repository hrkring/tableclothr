#' The list of available table themes
#'
#' @return The themes list
#' @export
#'
#' @examples available_themes()
available_themes <- function() {
  return(tableclothr:::themes$Theme.Name)
}
