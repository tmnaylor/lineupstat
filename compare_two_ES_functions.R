#'Comparing Effective Size: Base function for bootstrapping
#'
#'A base function for bootstrapping a dataframe of choices for 2 independent lineups
#'
#'@param linedf A dataframe of lineup data. Must consist of 2 columns, each
#'              containing data for 2 independent lineups
#'@param d Indices for bootstrap sample. Argument used by boot function to 
#'         select samples for bootstrapping
#'@details The approach here is to compute the effective size of each lineup
#'         separately, and to take the difference between them. This is then 
#'         bootstrapped, and if the bootstrap does not contain 0, we
#'          conclude the effective size estimates are different at p = alpha
#'@references Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
#'                  identification lineups. Law and Human Behavior, 5(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007).
#'                  Lineup construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read, & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'                  fairness. Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures
#'                  of lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'                  empirically assessing the fairness of a lineup. Law and Human Behavior, 3(4), 285-293.
#'@examples 
#'Data:

compare_eff_sizes.boot <- function(linedf, d){
    temp_df <- linedf[d,]
    diff <- (esize_T(table(temp_df[1])) - esize_T(table(temp_df[2])))
}


#'Master Function: Comparing Effective Size
#'
#'Function for comparing effective size of two indep lineups (Tredoux, 1998)
#'
#'@param linedf A dataframe of lineup data. Contains 2 columns, each of which
#'              hold data for 2 independent lineups
#'@returns Effective size, significance level, and confidence intervals (95%,
#'         Normal theory, percentile & bias-corrected)
#'@details This function is a master function, calling other functions
#'         it needs, and reporting results in some detail
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function to call when bootstrap resampling using boot function (in package 'boot')
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). Bootstrap methods and their
#'                  application. Cambridge University Press.
#'
#'            Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
#'                  identification lineups. Law and Human Behavior, 5(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007).
#'                  Lineup construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read, & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'                  fairness. Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures
#'                  of lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'                  empirically assessing the fairness of a lineup. Law and Human Behavior, 3(4), 285-293. 
#'                         
effsize_compare <- function(linedf){
    cat ("\n")
    temp1 <- boot(linedf, compare_eff_sizes.boot, R=1000) 
    temp2 <- boot.ci(temp1, type = c("norm","bca","perc"))  
    cat ("The two Effective sizes are ",esize_T(table(linedf[1]))," ", 
         esize_T(table(linedf[2])))
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



