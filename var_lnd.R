#' Variance of ln of diagnosticity ratio
#' 
#' Function to compute variance of ln(d) for k lineup pairs
#' 
#' @param linedf A dataframe of parameters for computing diagnosticity ratio
#' @details linedf dataframe is computed using the diag_param helper function 
#' 
#'          diag_param returns a dataframe containing the following:
#'          n11: Number of mock witnesses who identified the suspect in the target
#'              present condition
#'          n21: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'          n12: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'          n13: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
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
var_lnd <- function(linedf){
    var <- (linedf$n21/(linedf$n11+(linedf$n11+linedf$n21)))+
        (linedf$n22/(linedf$n12+(linedf$n12+linedf$n22)))
    var <- as.data.frame(var)
    return(var)
}