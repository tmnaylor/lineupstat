# Function for computing Functional Size (Wells, 1979)
# with both normal theory CI and bootstrap CI

# This function computes functional size, which is just
# N/D, where N = number of mock witnesses and D = number
# of mock witnesses choosing the suspect
func_size <- function(alinevec, susp_pos){
    fsize <- length(alinevec) / sum(alinevec == susp_pos)
}

# This function is a base function for the bootstrapping that ensues
# to compute the confidence intervals
func_size.boot <- function(alinevec, d=d, susp_pos){
    return (func_size(alinevec[d], susp_pos))
}

# This function is a master function, calling other functions
# it needs, and reporting results in some detail
func_size_report <- function(alinevec, susp_pos){
    cat ("\n")
    temp1 <- boot(alinevec, func_size.boot, susp_pos=3, R=1000) 
    temp2 <- boot.ci(temp1, type = c("norm","bca","perc"))  
    cat ("Functional size of lineup is ",round(func_size(alinevec,susp_pos),3))
    cat ("\n")
    cat ("Confidence intervals [95%]")
    cat ("\n")
    cat ("Normal Theory", round(temp2$normal[2:3],3))
    cat ("\n")    
    cat ("Bootstrap: percentile (R = 1000)", round(temp2$percent[4:5],3))
    cat ("\n")    
    cat ("Bootstrap: bias-corrected (R = 1000)", round(temp2$bca[4:5],3))
}

