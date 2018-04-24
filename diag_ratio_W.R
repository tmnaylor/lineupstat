#'Diagnosticity Ratio (Wells)
#'
#'Computes Wells' diagnosticity ratio for one lineup pair
#'
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param pos_pres Target position in TP lineup. Must be declared by user
#'@param pos_abs Target position in TA lineup. Must be declared by user
#'@examples
#'lineup_pres <- round(runif(100, 1, 6))
#'lineup_abs <- round(runif(100, 1, 6))
#'pos_pres <- c(1, 2, 3, 4, 5, 6)
#'pos_abs <- c(1, 2, 3, 4, 5, 6)
#'diag_ratio_W(lineup_pres, lineup_abs, pos_pres, pos_abs)
#'@references Wells, G. L., & Lindsay, R. C. L. (1980).
#'            On estimating the diagnosticity of eyewitness nonidentifications.
#'            Psychological Bulletin, 88, 776-784.
#'
#'            Wells, G. L., & Turtle, J. W. (1986). Eyewitness identification:
#'            The importance of lineup models. Psychological Bulletin, 99, 320-329.
#'
diag_ratio_W <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- sum(lineup_pres == pos_pres)/(length(lineup_pres))
    b <- sum(lineup_abs == pos_abs)/(length(lineup_abs))
    c <- a/b
    return(c)
}
