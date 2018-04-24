#' Ln of Diagnosticity Ratio
#' 
#' Computes ln of diagnosticity ratio: ln(d)
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
ln_diag_ratio <- function(linedf){
    d   <- (linedf$n11+0.5/((linedf$n11+linedf$n21)+0.5))/
           (linedf$n12+0.5/((linedf$n12+linedf$n22)+0.5))
    lnd <- log(d)
    lnd <- as.data.frame(lnd)
    return(lnd)
}