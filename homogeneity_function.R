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
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(100,1,6))
#'C <-  round(runif(100,1,6))
#'df1 <- cbind(A, B, C)
#'df1 <- as.data.frame(df1)
#'lineup_pres_list <- as.list(df1)
#'pos_pres <- c(1, 2, 3, 4, 5, 6)
#'
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(100,1,6))
#'C <-  round(runif(100,1,6))
#'df2 <- cbind(A, B, C)
#'df2 <- as.data.frame(df2)
#'lineup_abs_list <- as.list(df2)
#'pos_abs <- c(1, 2, 3, 4, 5, 6)
#'
#'homog_diag(lineup_pres_list, lineup_abs, list, pos_pres, pos_abs)
#'
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. Law and Human Behavior, 5(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. Law and Human Behavior,
#'            3(4), 285-293.
#'            
homog_diag <- function(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs){
  linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs)
  par1 <- ln_diag_ratio(linedf)
  par2 <- var_lnd(linedf)
  par3 <- d_weights(linedf)
  par4 <- cbind(par1, par2, par3)
  par5 <- chi_diag(par4)
  pchisq(par5, ncol(linedf)-1, lower.tail=F)

}
