#Base function for computing bootstrapped effective size

esize_m_boot <- function (linevec, d, printarg=FALSE){
    linetable <- table(linevec[d])
    k <- length(linetable)
    ea <- sum(linetable)/k
    x <- sum(abs(linetable-ea)/(2*ea))
    esize_ma = k-x
    ka <- sum(linetable!=0)
    linetable_a <- linetable[linetable!=0]
    ea <- (sum(linetable))/ka
    xa <- sum(abs(linetable_a-ea)/(2*ea))
    esize_ma_a = ka-xa
    if (printarg) {
        cat("Effective size (Malpass, 1981) = ", esize_ma_a,"\n")
        cat("Effective size (Malpass, 1981,","\n", 
            "            adj Tredoux, 1998) = ", esize_ma, "\n")
    }
    esize_ma
}