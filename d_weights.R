#'Diagnosticity ratio weights
#'
#'Function to compute weights of each diagnosticity ratio  for k lineup pairs
#'@param linedf A dataframe of parameters for computing diagnosticity ratio
#'@details linedf dataframe is computed using the diag_param helper function 
#' 
#'          diag_param returns a dataframe containing the following:
#'          n11: Number of mock witnesses who identified the suspect in the target
#'              present condition
#'          n21: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'          n12: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'          n13: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition\
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237. 

d_weights <- function(linedf){
    numerator   <- linedf$n11*linedf$n12*(linedf$n11+linedf$n21)*
                   (linedf$n12+linedf$n22)
    denominator <- linedf$n11*linedf$n22*(linedf$n11+linedf$n21)+
                   linedf$n12*linedf$n21*(linedf$n12+linedf$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)
    
}