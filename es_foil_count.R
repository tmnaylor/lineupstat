#'Effective Size?
#'
#'Function for computing effective size (Malpass, 1981) counting foils who fall
#'within the CI for chance guessing
#'@param lineup_vec A numeric vector of lineup choices
#'@param susp_pos A numeric vector indexing all lineup members
#'@examples
#'lineup_vec <- round(runif(100, 1, 6))
#' susp_pos <- c(1, 2, 3, 4, 5, 6)
#'es_foil_count(lineup_vec, susp_pos)
#'@references Malpass, R S. (1981). Effective size and defendant bias in eyewitness
#'            identification lineups. Law and Human Behavior, 5, 299-309.
#'
es_foil_count <- function(lineup_vec, susp_pos){
  ci <- lineup_boot_allprop(lineup_vec, susp_pos)
  k <- 1/length(susp_pos)
  ci_count <- cbind(ci[,1] <= k & ci[,2] >= k)
  print(sum(ci_count == TRUE))
}
