#' Master function: Homogeneity of diagnosticity ratio
#'
#' This function provides assesses the homogeneity of the diagnosticity ratio of
#'  k lineup pairs
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        suspect was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       suspect was absent
#'@param pos_pres A numeric vector indexing lineup member positions for the target
#'          present condition
#'@param pos_abs A numeric vector indexing lineup member positions for the target
#'         absent condition
#'
#'@examples

homog_diag <- function(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs){
  linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs)
  par1 <- ln_diag_ratio(linedf)
  par2 <- var_lnd(linedf)
  par3 <- d_weights(linedf)
  par4 <- cbind(par1, par2, par3)
  par5 <- chi_diag(par4)
  pchisq(par5, ncol(linedf)-1, lower.tail=F)

}
