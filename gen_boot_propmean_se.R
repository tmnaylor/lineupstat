#'Descriptive statistics for bootstrapped lineup proportion
#'
#'Function for computing mean. med and se of boot proportion
#'@param lineuprops A dataframe of bootstrapped lineup proportions
#'@returns Mean, median and standard error of lineup proportion across a bootstrapped 
#'         dataframe
#'@examples
#'
#'Data: Get bootstrapped data --> no function for this
#'      Have been using boot package to get cis
gen_boot_propmean_se <- function (lineuprops){
    mean_boot_prop = mean(lineuprops, na.rm = T)
    median_boot_prop = median(lineuprops, na.rm = T)
    stdev_boot_prop = sd(lineuprops, na.rm = T)
    n = length(lineuprops)
    std_error_boot_prop    = stdev_boot_prop/sqrt(n)
    ci025 = gen_boot_propci(lineuprops,.025)
    ci975 = gen_boot_propci(lineuprops,.975)
    cat("Boot prop. (mean)   = ", mean_boot_prop,"\n")
    cat("Boot prop. (median) = ", median_boot_prop,"\n")
    cat("SD of boot prop     = ", stdev_boot_prop,"\n")
    cat("SE of boot prop     = ", stdev_boot_prop/sqrt(n),"\n")
    cat("2.5% boot CI lvl    = ", ci025,"\n")
    cat("97.5% boot CI lvl   = ", ci975,"\n")
}