#Function to compute variance of ln(d) for k lineups
var_lnd <- function(df){
    var <- (df$n21/(df$n11+(df$n11+df$n21)))+(df$n22/(df$n12+(df$n12+df$n22)))
    var <- as.data.frame(var)
    return(var)
}