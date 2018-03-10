# Computing ln(d) for k lineups
# Needs df of diagnosticity parameters --> get using helper fx
# OK, but let's say what the helper fx is and what it computes

ln_diag_ratio <- function(df){
    d   <- (df$n11+0.5/((df$n11+df$n21)+0.5))/
           (df$n12+0.5/((df$n12+df$n22)+0.5))
    lnd <- log(d)
    lnd <- as.data.frame(lnd)
    
    return(lnd)
}