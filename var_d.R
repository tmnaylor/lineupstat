#' Variance of diagnosticity ratio (Tredoux)
#' 
#' Computes the variance of the diagnosticity ratio for a lineup pair
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which 
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which 
#'                   the target was absent
#'@param pos_pres Target position in TP lineup. Must be declared by user
#'@param pos_abs Target position in TA lineup. Must be declared by user
#'@examples
#'
#'#'lineup_pres <- round(runif(100, 1, 6))
#'lineup_abs <- round(runif(100, 1, 6))
#'pos_pres <- c(1, 2, 3, 4, 5, 6)
#'pos_abs <- c(1, 2, 3, 4, 5, 6)
#'var_diag_ratio_T(lineup_pres, lineup_abs, pos_pres, pos_abs)

var_diag_ratio <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- sum(lineup_pres  != pos_pres)
    b <- sum(lineup_pres == pos_pres)*(length(lineup_pres))
    c <- sum(lineup_abs  != pos_abs)
    d <- sum(lineup_abs == pos_abs)*(length(lineup_abs))
    e <- (a/b)+(c/d)
    return(e)
}