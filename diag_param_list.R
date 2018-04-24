#'Parameters for diagnosticity ratio
#'
#'This function calculates the parameters needed to calculate the diagnosticity
#'         ratio for several lineup pairs.
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        suspect was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       suspect was absent
#'@param pos_pres A numeric vector indexing lineup member positions for the target
#'          present condition
#'@param pos_abs A numeric vector indexing lineup member positions for the target
#'         absent condition
#'@returns A dataframe containing:
#'         n11: Number of mock witnesses who identified the suspect in the target
#'              present condition
#'         n21: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'         n12: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'         n13: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
#'@details Lineup pairs consist of one lineup in which the target was present (TP)
#'         and one lineup in which the target was absent (TA).
#'
#'         Each lineup pair must occupy corresponding positions in the TA and TP lists.
#'
#'         Example:
#'
#'         For a lineup pair A that consists of (1)TP lineup and (2)TA lineup:
#'         A(1) is the first vector in the TP list
#'         A(2) is the first vector in the TP list
#'
diag_param <- function(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs){
  diagdf1 <- as.data.frame(matrix(ncol = 2,
                                  nrow = length(lineup_pres_list)))

  for (i in 1:length(lineup_pres_list)){
    diagdf1[i,1]= sum(lineup_pres_list[[i]] == pos_pres[i])
    diagdf1[i,2] = sum(lineup_pres_list[[i]] != pos_pres[i])


  }

  diagdf2 <- as.data.frame(matrix(ncol = 2,
                                  nrow = length(lineup_abs_list)))
  for (i in 1:length(lineup_abs_list)){
    diagdf2[i,1]= sum(lineup_abs_list[[i]] == pos_abs[i])
    diagdf2[i,2] = sum(lineup_abs_list[[i]] != pos_abs[i])

    diagdf <- cbind(diagdf1, diagdf2)
    names(diagdf) <- c("n11", "n21", "n12", "n22")
    diagdf = as.data.frame(sapply(diagdf, as.numeric))
  }
  return(diagdf)
}
