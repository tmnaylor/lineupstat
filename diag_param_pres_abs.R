#Function to compute n11/12 and n21/22 from df of present/absent lineup choices
#function works for 2 lists at a time - present/absent. 
#Present & absent elements must correspond 
#Function takes 2 lists of varying lengths (one for present, one for absent) 
#and 2 corresponding vectors of suspect positions 
#(each column in df corresponds to row in vector)
#list_pres = list of vectors containing choices for target present lineups
#list_abs = list of vectors containing choices for rarget absent lineup
#pos_pres = vector of suspect positions in target present lineups
#pos_abs = vector of suspect positions in target absent lineups
#position of lineups to be compared must correspond across ta/tp lists

diag_param <- function(list1, list2, pos1, pos2){
    diagdf1 <- as.data.frame(matrix(ncol = 2, 
                                    nrow = length(list1)))
    
    for (i in 1:length(list1)){
        diagdf1[i,1]= sum(list1[[i]] == pos1[i])
        diagdf1[i,2] = sum(list1[[i]] != pos1[i])
        
        
    }
    
    diagdf2 <- as.data.frame(matrix(ncol = 2, 
                                    nrow = length(list2)))
    for (i in 1:length(list2)){
        diagdf2[i,1]= sum(list2[[i]] == pos2[i])
        diagdf2[i,2] = sum(list2[[i]] != pos2[i]) 
        
        diagdf <- cbind(diagdf1, diagdf2)
        names(diagdf) <- c("n11", "n21", "n12", "n22")
        diagdf = as.data.frame(sapply(diagdf, as.numeric))
    }
    return(diagdf)
}