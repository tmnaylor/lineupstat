# Function for comparing effective size of two indep lineups (Tredoux, 1998)
# The approach here is to compute the effective size of each lineup
# separately, and to take the difference between them.
# We then bootstrap this, and if the bootstrap does not contain 0, we
# conclude the effective size estimates are different at p = alpha

compare_eff_sizes.boot <- function(linedf, d){
    temp_df <- linedf[d,]
    diff <- (esize_T(table(temp_df[1])) - esize_T(table(temp_df[2])))
}


# This function is a master function, calling other functions
# it needs, and reporting results in some detail
func_size_report <- function(adf){
    cat ("\n")
    temp1 <- boot(adf, compare_eff_sizes.boot, R=1000) 
    temp2 <- boot.ci(temp1, type = c("norm","bca","perc"))  
    cat ("The two Effective sizes are ",esize_T(table(adf[1]))," ", 
         esize_T(table(adf[2])))
    cat ("\n")
    cat ("If the interval includes 0, ns at p = .05")
    cat ("\n")
    cat ("Confidence intervals of difference [95%]")
    cat ("\n")
    cat ("Normal Theory", round(temp2$normal[2:3],3))
    cat ("\n")    
    cat ("Bootstrap: percentile (R = 1000)", round(temp2$percent[4:5],3))
    cat ("\n")    
    cat ("Bootstrap: bias-corrected (R = 1000)", round(temp2$bca[4:5],3))
}



