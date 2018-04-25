#'Parameters for diagnosticity ratio
#'
#'This function calculates the parameters needed to calculate the diagnosticity
#'         ratio for several lineup pairs.
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_pres A list containing k numeric vectors indexing lineup member positions 
#'                for the target present condition
#'@param pos_abs A list containing k numeric vectors indexing lineup member positions 
#'               for the target absent condition
#'@returns A dataframe containing:
#'         n11: Number of mock witnesses who identified the target in the target
#'              present condition
#'         n21: Number of mock witnesses who did not identify the target in the
#'              target present condition
#'         n12: Number of mock witnesses who identified the target in the target
#'              absent condition
#'         n13: Number of mock witnesses who did not identify the target in the
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
#'         Data must be in a list format. This allows the function to compare 
#'         lineups in which the number of choices and number of lineup members differs. 
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup 
#'            fairness. Law and Human Behavior, 22(2), 217-237.
#'@examples
#'
#'#Target present data
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4)) 
#'lineup_pos_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'a1 <- c(1, 2, 3, 4, 5, 6)
#'b1 <- c(1, 2, 3, 4, 5)
#'c1 <- c(1, 2, 3, 4)
#'pos_pres <- list(a1, b1, c1)
#'rm(a1, b1, c1)
#'
#'Target absent data
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4)) 
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'a1 <- c(1, 2, 3, 4, 5, 6)
#'b1 <- c(1, 2, 3, 4, 5)
#'c1 <- c(1, 2, 3, 4)
#'pos_abs <- list(a1, b1, c1)
#'rm(a1, b1, c1)
#'
#'diag_param(lineup_pres_list, lineup_abs_list, pos_pres, abs_pres)

diag_param <- function(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs){
  diagdf1 <- as.data.frame(matrix(ncol = 2,
                                  nrow = length(lineup_pres_list)))

  for (i in 1:length(lineup_pres_list)){
    diagdf1[i,1]= sum(lineup_pres_list[[i]] == pos_pres[[i]])
    diagdf1[i,2] = sum(lineup_pres_list[[i]] != pos_pres[[i]])


  }

  diagdf2 <- as.data.frame(matrix(ncol = 2,
                                  nrow = length(lineup_abs_list)))
  for (i in 1:length(lineup_abs_list)){
    diagdf2[i,1]= sum(lineup_abs_list[[i]] == pos_abs[[i]])
    diagdf2[i,2] = sum(lineup_abs_list[[i]] != pos_abs[[i]])

    diagdf <- cbind(diagdf1, diagdf2)
    names(diagdf) <- c("n11", "n21", "n12", "n22")
    diagdf = as.data.frame(sapply(diagdf, as.numeric))
  }
  return(diagdf)
}

