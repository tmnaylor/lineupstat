#Function to compute weights for diagnosticity ratios in k lineup pairs

d_weights <- function(df){
    numerator   <- df$n11*df$n12*(df$n11+df$n21)*(df$n12+df$n22)
    denominator <- df$n11*df$n22*(df$n11+df$n21)+
                   df$n12*df$n21*(df$n12+df$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)
    
}