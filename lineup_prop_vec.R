#'Lineup proportion
#'
#'Computes the proportion of mock witnesses identifying a particular lineup member
#'@param lineup_vec A numeric vector of lineup choices
#'@param susp_pos Suspect/lineup member position. Must be declared by user
#'@return Returns a proportion indicating the frequency with which a lineup
#'        member was selected
#'@examples
#'lineup_prop_vec(lineup_vec, 3)
#'lineup_prop_vec(c(1, 2, 5, 3, 2, 4), 2)

lineup_prop_vec <- function(lineup_vec, susp_pos){
    sum(lineup_vec == susp_pos)/length(lineup_vec)
}


