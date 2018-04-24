#'Lineup proportion
#'
#'Computes the proportion of mock witnesses identifying a particular lineup member
#'@param lineup_table A table of lineup choices
#'@param susp_pos Suspect/lineup member position. Must be declared by user
#'@return Returns a proportion indicating the frequency with which a lineup
#'        member was selected
#'@examples
#'lineup_prop_vec(lineup_table, 3)
#'lineup_prop_vec(table(lineup_vec), 2))
#'lineup_prop_vec(table(c(1, 2, 5, 3, 2, 4)), 2)

lineup_prop_tab <- function (lineup_table,susp_pos){
    lineup_table[susp_pos]/sum(lineup_table)
}


