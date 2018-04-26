#'Bootstrapped lineup proportion
#'
#'Base function for computing bootstrapped lineup proportion for a lineup member
#'@param lineup_vec A numeric vector of lineup choices
#'@param d=d Indices for bootstrap sample. Argument used by boot function to 
#'           select samples for bootstrapping
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function to call when bootstrap resampling using boot function
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). Bootstrap methods and their
#'            application. Cambridge University Press

lineup_prop_boot <- function(lineup_vec, d,  target_pos){
    sum(lineup_vec[d] == target_pos)/length(lineup_vec)
}
