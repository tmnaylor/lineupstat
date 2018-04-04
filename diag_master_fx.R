#Master function for computing homogeneity of k diagnosticity ratios
#Template function - still needs work -- cis, proper naming and documentation


diag_master_fx<- function(list1, list2, pos1, pos2){
    
    linedf <- diag_param(adf1, adf2, apos1, apos2)
    par1 <- ln_diag_ratio(linedf)
    par2 <- var_lnd(linedf)
    par3 <- d_weights(linedf)
    par4 <- cbind(par1, par2, par3)
    par5 <- chi_diag(par4, d_bar(par4))
    return(pchisq(par5, ncol(linedf)-1, lower.tail = F))
    
}