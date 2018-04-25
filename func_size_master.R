#'Functional Size with Bootstrapped Confidence Intervals
#'
#'This function is a master function, calling other functions it needs, 
#'and reporting results in some detail
#'
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function calls functions from package 'boot'
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). Bootstrap methods and their
#'            application. Cambridge University Press.
#'            
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness.Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. Law and Human Behavior,
#'            3(4), 285-293.
#'
func_size_report <- function(lineup_vec, target_pos){
    cat ("\n")
    temp1 <- boot(lineup_vec, func_size.boot, target_pos=3, R=1000) 
    temp2 <- boot.ci(temp1, type = c("norm","bca","perc"))  
    cat ("Functional size of lineup is ",round(func_size(lineup_vec,target_pos),3))
    cat ("\n")
    cat ("Confidence intervals [95%]")
    cat ("\n")
    cat ("Normal Theory", round(temp2$normal[2:3],3))
    cat ("\n")    
    cat ("Bootstrap: percentile (R = 1000)", round(temp2$percent[4:5],3))
    cat ("\n")    
    cat ("Bootstrap: bias-corrected (R = 1000)", round(temp2$bca[4:5],3))
}