#'Bootstrapped Effective Size
#'
#'Base function for computing bootstrapped effective size
#'@param lineup_vec A vector of lineup choices
#'@param d Indices for bootstrap resampling
#'@param printarg Defaults to FALSE. If TRUE, provides both Tredoux's (1998)
#'                and Malpass's (1981) calculations of effective size
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function to call when bootstrap resampling using boot function (in package 'boot')
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). Bootstrap methods and their
#'            application. Cambridge University Press.
#'            
#'            Malpass, R. S. (1981). Effective size and defendant bias in
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

esize_m_boot <- function (lineup_vec, d, printarg=FALSE){
    lineup_table <- table(lineup_vec[d])
    k <- length(lineup_table)
    ea <- sum(lineup_table)/k
    x <- sum(abs(lineup_table-ea)/(2*ea))
    esize_ma = k-x
    ka <- sum(lineup_table!=0)
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