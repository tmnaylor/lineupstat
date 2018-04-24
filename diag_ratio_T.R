#' Diagnosticty Ratio (Tredoux, 1998)
#'
#'Computes Tredoux's diagnosticity ratio for one lineup pair
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param pos_pres Target position in TP lineup. Must be declared by user
#'@param pos_abs Target position in TA lineup. Must be declared by user
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237.
#'@examples
#'lineup_pres <- round(runif(100, 1, 6))
#'lineup_abs <- round(runif(100, 1, 6))
#'pos_pres <- c(1, 2, 3, 4, 5, 6)
#'pos_abs <- c(1, 2, 3, 4, 5, 6)
#'diag_ratio_T(lineup_pres, lineup_abs, pos_pres, pos_abs)

diag_ratio_T <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- (sum(lineup_pres == pos_pres) + 0.5)/(length(lineup_pres) + 0.5)
    b <- (sum(lineup_abs == pos_abs) + 0.5)/(length(lineup_abs) + 0.5)
    c <- a/b
    return(c)
}
