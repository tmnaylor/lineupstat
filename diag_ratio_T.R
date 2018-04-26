#' Diagnosticty Ratio (Tredoux, 1998)
#'
#'Computes Tredoux's diagnosticity ratio for one lineup pair
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param pos_res A scalar, representing target position in TP lineup. Must be declared by user
#'@param pos_abs A scalar, representing target position in TA lineup. Must be declared by user
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
#'@examples
#'Data:
#'lineup_pres <- round(runif(100, 1, 6))
#'lineup_abs <- round(runif(70, 1, 5))
#'pos_pres <- 3
#'pos_abs <- 5
#'
#'Call:
#'diag_ratio_T(lineup_pres, lineup_abs, pos_pres, pos_abs)
#'diag_ratio_T(lineup_pres, lineup_abs, 3, 5)

diag_ratio_T <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- (sum(lineup_pres == pos_pres) + 0.5)/(length(lineup_pres) + 0.5)
    b <- (sum(lineup_abs == pos_abs) + 0.5)/(length(lineup_abs) + 0.5)
    c <- a/b
    return(c)
}
