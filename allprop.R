#' Lineup proportion for all lineup members
#'
#' Computes lineup proportion for each member in a lineup
#' @param lineup_vec A numeric vector of lineup choices
#' @param susp_pos A numeric vector indexing all lineup members
#' @return Returns a vector containing lineup proportion for each lineup member
#' @examples
#' lineup_vec <- round(runif(100, 1, 6))
#' susp_pos <- c(1, 2, 3, 4, 5, 6)
#' allprop(lineup_vec, susp_pos)
#' @references Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'             assessing the fairness of a lineup. Law and Human Behavior, 3(4),
#'             285-293.
#'
allprop <- function(lineup_vec, susp_pos){
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(susp_pos)))
    for (i in 1:length(susp_pos)){
        propvec[i,]=lineup_prop_vec(lineup_vec, susp_pos[i])
    }
    return(propvec)
}
