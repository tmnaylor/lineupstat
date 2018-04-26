#'Effective Size
#'
#'Function for computing Effective Size (Malpass, 1981)
#' @param lineup_table A table of lineup choices
#' @param printarg Defaults to FALSE. If TRUE, provides both Tredoux's (1998)
#'                 calculation of effective size and Malpass's (1981) calculation 
#'                 of effective size
#' @details Reduces the size of a lineup from a (corrected) nominal starting
#'          value by the degree to which members are, in sum, chosen below
#'          the level of chance expectation.
#' @references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. Law and Human Behavior, 5(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. Law and Human Behavior,
#'            3(4), 285-293.
#'@examples
#'Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_table <- table(lineup_vec)
#'
#'Call:
#'e <- esize_m(lineup_table, printarg = TRUE)
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
