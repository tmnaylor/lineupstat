#'Effective Size
#'
#'Function for computing Effective Size (Malpass, 1981)
#' @param lineup_table
#' @param printarg Defaults to FALSE. If TRUE, provides both Tredoux's (1998)
#' calculation of effective size and Malpass's (1981) calculation of effective size
#' @details
#' @references
esize_m <- function (lineup_table, printarg=FALSE){
  k <- length(lineup_table)
  ea <- sum(lineup_table)/k
  x <- sum(abs(lineup_table-ea)/(2*ea))
  esize_ma = k-x
  ka <- sum(linetable!=0)
  lineup_table_a <- lineup_table[lineup_table!=0]
  ea <- (sum(lineup_table))/ka
  xa <- sum(abs(lineup_table_a-ea)/(2*ea))
  esize_ma_a = ka-xa
  if (printarg) {
    cat("Effective size (Malpass, 1981) = ", esize_ma_a,"\n")
    cat("Effective size (Malpass, 1981,","\n",
        "            adj Tredoux, 1998) = ", esize_ma, "\n")
  }
  esize_ma
}
