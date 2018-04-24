#'Lineup proportion over dataframe
#'
#'Function for computing lineup proportion over a dataframe
#'@param lineup_boot_df Dataframe of lineup choices (usually a bootstrapped set)
#'@param susp_pos Suspect/lineup member position. Must be declared by user
#'@examples
#'lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)
#'susp_pos <- 3
#'gen_lineup_prop(lineup_boot_df, susp_pos)
#'gen_lineup_prop(lineup_boot_df, 3)
#'
gen_lineup_prop <- function (lineup_boot_df, susp_pos){
  map(lineup_boot_df,~table(.)) %>%
    map_dbl(., ~ lineup_prop_tab(.,susp_pos))
}
