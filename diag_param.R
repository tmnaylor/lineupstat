#Function to compute n11/12 and n21/22 from df of present/absent lineup choices
#function works for 1 df at a time - present/absent. 
#Present & absent dfs must correspond (1:1)
#Function takes a dataframe and a vector of suspect positions (each column in df corresponds to each row in vector)
diag_param <- function(df, pos){
    diagdf <- as.data.frame(matrix(ncol = 2, 
                                    nrow = length(df)))
    
    for (i in 1:ncol(df)){
        diagdf[i,1]= sum(df[i] == pos[i])
        diagdf[i,2] = sum(df[i] != pos[i])
        
    }
    return(diagdf)
}