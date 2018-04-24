#'Mean diagnosticity ratio for k lineup pairs
#'
#'Function for computing pooled estimator from a set of k diagnosticity ratios
#'
#'@param df A dataframe containing: ln(d), variance of ln(d), d weights
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237.
#'@examples
#'ratio <- ln_diag_ratio(linedf)  
#'var <- var_lnd(linedf)
#'wi <- d_weights(linedf)
#'df <- cbind(ratio, var, wi)
#'d_bar(df)
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

d_bar <- function(df){
    numerator   <- sum(df$wi*df$lnd)
    denominator <- sum(df$wi)
    d_bar       <- exp(numerator/denominator)
    return(d_bar)
}