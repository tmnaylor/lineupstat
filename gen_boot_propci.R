# Function for computing arbitrary percentile of boot proportion
# assuming a bootstrap sample df, and perc as argument
gen_boot_propci <- function (lineuprops, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineuprops, probs = perc)
    }
    else {
        cat("Illegal value entered (must be proportion)")
    }
}