#'Confidence intervals for lineup proportion
#'
#'Computes bootstrapped confidence intervals for lineup proportion
#'@param lineup_vec A numeric vector of lineup choices
#'@param susp_pos A numeric vector indexing all lineup members
#'@return Returns a vector of bias corrected confidence intervals for lineup proportion
#'        for each member in a lineup
#'@examples
#'lineup_vec <- round(runif(100, 1, 6))
#' susp_pos <- c(1, 2, 3, 4, 5, 6)
#'lineup_boot_allprop(lineup_vec, susp_pos)
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function to call when bootstrap resampling using boot function (in package 'boot')
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). Bootstrap methods and their
#'            application. Cambridge University Press.
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            assessing the fairness of a lineup. Law and Human Behavior, 3(4),
#'            285-293.

lineup_boot_allprop <- function(lineup_vec, susp_pos){
  z <- map(pos,~boot(lineup_vec, lineup_prop_boot, susp_pos = .x, R = 1000) %>%
             boot.ci(type = "bca")) %>%
    map(magrittr::extract, "bca") %>%
    map_df(magrittr::extract,"bca")

  z2 <-  matrix(ncol = 6,nrow = 5, z$bca) %>%
    data.frame() %>%
    slice(4:5)
  ci <- as.data.frame(t(z2))

  return(ci)
}
