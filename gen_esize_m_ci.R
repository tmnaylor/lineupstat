#'Bootstrapped Confidence Intervals for Effective Size
#'
#'Function for computing Effective Size (Malpass, 1981, as adjusted by Tredoux, 1998)
#'with CIs from bootstrap df of lineups
#'@param lineupsizes A dataframe of bootstrapped effective sizes
#'@param perc Defaults to 0.5. Can be specified by user, according to desired 
#'            alpha level
gen_esize_m_ci <- function (lineupesizes, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineupesizes, probs = perc)
    }
    else {
        cat("Illegal value entered (perc must be proportion)")
    }
}