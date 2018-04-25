#'Rotate vector
#'
#'Helper function to rotate vector
#'
#'@param lineup_vec
rot_vector <- function(lineup_vec){
  lineup_vec1 <- lag(lineup_vec)
  lineup_vec1[1] <- lineup_vec[length(lineup_vec)]
  lineup_vec <- lineup_vec1
}
