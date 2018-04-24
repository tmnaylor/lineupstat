#'Effective Size (across a dataframe)
#'
#'  Function for computing Effective Size (Tredoux, 1998) on
#'  lineups contained as columns in a df, usually from a bootstrapped sample
#'  @param lineup_boot_df A dataframe containing bootstrapped samples of lineup data
#'  @details
#'  @references
#'
gen_esize_T <- function (lineup_boot_df){
  map(lineup_boot_df,~table(.)) %>%
    map_dbl(., ~ esize_T(.))
}
