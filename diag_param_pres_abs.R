#Function to compute n11/12 and n21/22 from df of present/absent lineup choices
#function works for 2 dfs at a time - present/absent. 
#Present & absent dfs must correspond (1:1)
#Function takes 2 dataframes and 2 corresponding vectors of suspect positions 
#(each column in df corresponds to row in vector)
# OK THIS SEEMS TO WORK FINE

diag_param <- function(df1, df2, pos1, pos2){
    diagdf1 <- as.data.frame(matrix(ncol = 2, 
                                    nrow = length(df1)))
    
    for (i in 1:ncol(df1)){
        diagdf1[i,1] = sum(df1[i] == pos1[i])
        diagdf1[i,2] = sum(df1[i] != pos1[i])
        
        diagdf2 <- as.data.frame(matrix(ncol = 2, 
                                        nrow = length(df2)))
    }
    
    for (i in 1:ncol(df2)){
        diagdf2[i,1] = sum(df2[i] == pos2[i])
        diagdf2[i,2] = sum(df2[i] != pos2[i]) 
        
        diagdf        <- cbind(diagdf1, diagdf2)
        names(diagdf) <- c("n11", "n21", "n12", "n22")
        
    }
    return(diagdf)
}