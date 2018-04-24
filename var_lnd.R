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

var_lnd <- function(linedf){
    var <- (linedf$n21/(linedf$n11+(linedf$n11+linedf$n21)))+
        (linedf$n22/(linedf$n12+(linedf$n12+linedf$n22)))
    var <- as.data.frame(var)
    return(var)
}