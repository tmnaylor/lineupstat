#'Bootstrapped Functional Size
#'
#'This function is a base function for the bootstrapping that ensues
#'to compute the confidence intervals
#'@param lineup_vec A numeric vector of lineup choices
#'@param d Iindices for bootstrap resampling
#'@param susp_pos A scalar, representing position of target in lineup. Must be declared by user
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function to call when bootstrap resampling using boot function (in package 'boot')
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
func_size.boot <- function(lineup_vec, d=d, susp_pos){
    return (func_size(lineup_vec[d], susp_pos))
}