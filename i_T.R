#' I (Tredoux, 1998)
#'
#' Function for generating I component of effective size
#' @param lineup_table A table of lineup choices
#' @references
#' @details
i_T <- function(lineup_table){
  i <- 1-(1/(sum(lineup_table)^2))*sum(lineup_table^2)
}
