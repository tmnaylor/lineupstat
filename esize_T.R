#'Effective Size (Tredoux, 1998)
#'
#' Function for generating Effective Size (Tredoux, 1998)
#' @param lineup_table A table of lineup choices
#' @details
#' @references
#'
esize_T <- function(lineup_table){
  i <- 1-(1/(sum(lineup_table)^2))*sum(lineup_table^2)
  i <- 1/(1-i)
}
