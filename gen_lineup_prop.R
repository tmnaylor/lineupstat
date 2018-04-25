#'Lineup proportion over dataframe
#'
#'Function for computing lineup proportion over a dataframe
#'@param lineup_boot_df Dataframe of lineup choices (usually a bootstrapped set)
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@returns A vector of bootstrapped proportions
#'@examples
#'lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)
#'target_pos <- 3
#'gen_lineup_prop(lineup_boot_df, target_pos)
#'bootprops <- gen_lineup_prop(lineup_boot_df, 3)
#'
gen_lineup_prop <- function (lineup_boot_df, target_pos){
  map(lineup_boot_df,~table(.)) %>%
    map_dbl(., ~ lineup_prop_tab(.,target_pos))
}
