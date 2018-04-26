# Function for computing Effective Size (Tredoux, 1998)
# With CIs from bootstrap df of lineups
gen_esize_T_ci <- function (lineupesizes, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineupesizes, probs = perc)
    }
    else {
        cat("Illegal value entered (perc must be proportion)")
    }
}