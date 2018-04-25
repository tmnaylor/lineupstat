#'Homogeneity of diagnosticity ratio with bootstrapped CIs
#'
#'Function for computing bootstrapped estimates of homogeneity of diagnosticity ratio
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_pres A numeric vector indexing lineup member positions for the target
#'                present condition
#'@param pos_abs A numeric vector indexing lineup member positions for the target
#'               absent condition
#'@details Computes bootstrapped diagnosticity ratio with chi-squared estimate,
#'         significance level and confidence intervals for k lineup pairs
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

homog_diag_boot <- function(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs, B){
    pres_bootdf <- gen_boot_samples_list(lineup_pres_list, B)
    abs_bootdf <- gen_boot_samples_list(lineup_abs_list, B)
    linedf <- diag_param_boot(pres_bootdf, abs_bootdf, pos_pres, pos_abs)
    par1 <- ln_diag_ratio(linedf)
    par2 <- var_lnd(linedf)
    par3 <- d_weights(linedf)
    par4 <- cbind(par1, par2, par3)
    par5 <- d_bar(par4)
    par6 <- chi_diag(par4, par5)
    cat("Mean diagnosticity ratio:", par5)
    cat("\n")
    cat("Chi-square estimate (q):", par6)
    cat("\n")
    cat("Sig:",pchisq(par6, ncol(linedf)-1, lower.tail=F))
}