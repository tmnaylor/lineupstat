#'Diagnosticity ratio weights
#'
#'Function to compute weights of each diagnosticity ratio  for k lineup pairs
#'@param linedf A dataframe of parameters for computing diagnosticity ratio
#'@details In order to obtain a pooled estimate of a set of diagnosticity ratios,
#'         we use a weight for each ratio that is equal to the inverse of its
#'         variance.
#'
#'         linedf dataframe is computed using the diag_param helper function,
#'          diag_param: returns a dataframe containing the following:
#'          n11: Number of mock witnesses who identified the suspect in the target
#'              present condition
#'          n21: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'          n12: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'          n13: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
#'              
#'          
#'@returns A dataframe with one column containing weights for each pair of
#'         lineups for which diagnosticity ratio is being calculated. 
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237.
#'@examples
#'Use diag_param helper function to get linedf: produces a dataframe containing
#'the parameters needed to calculate diagnosticity ratio (see documentation for 
#'diag_param)
#'
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_pres, abs_pres)
#'wi <- d_weights(linedf)
d_weights <- function(linedf){
    numerator   <- linedf$n11*linedf$n12*(linedf$n11+linedf$n21)*
                   (linedf$n12+linedf$n22)
    denominator <- linedf$n11*linedf$n22*(linedf$n11+linedf$n21)+
                   linedf$n12*linedf$n21*(linedf$n12+linedf$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)
    
}