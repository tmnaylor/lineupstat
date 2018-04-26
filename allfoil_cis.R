#'Confidence Intervals for Proportion
#'
#'Function to compute ci high for each foil in a lineup
#'@param lineuptabprops A dataframe of bootstrapped lineup proportions
#'@param sumlineup
#'@details see Malpass, 1981

allfoil_cihigh <- function(linetabprops, sumlineup){
    z <- 1:length(linetabprops)
    for (i in 1:length(linetabprops)){
        z[i] <-  boot0975(linetabprops[i],sumlineup)
    }
    z
}

allfoil_cilow <- function(linetabprops, sumlineup){
    z <- 1:length(linetabprops)
    for (i in 1:length(linetabprops)){
        z[i] <-  boot025(linetabprops[i],sumlineup)
    }
    z
}