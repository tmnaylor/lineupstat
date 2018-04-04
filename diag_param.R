# Function to compute n11/12 and n21/22 from df of present/absent lineup choices
# function works for 1 df at a time - present/absent. 
# Present & absent dfs must correspond (1:1)
# Function takes a dataframe and a vector of suspect positions 
# (each column in df corresponds to each row in vector)
# TAMSYN: I think we need more detail here
# What is in df?  Is it a df containing columns that are total
# choices of a lineup member by witnesses? Or is it a raw
# vector of choices? The function seems to work for the latter
# but not the former
# For later error checking, pos values can only take on certain values
# i.e. max = number of lineup members
diag_param <- function(df, pos){
    diagdf <- as.data.frame(matrix(ncol = 2, 
                                   nrow = length(df)))
    
    for (i in 1:ncol(df)){
          diagdf[i,1] = sum(df[i] == pos[i])
          diagdf[i,2] = sum(df[i] != pos[i])
        
    }
    return(diagdf)
}