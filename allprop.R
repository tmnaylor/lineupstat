#Function for computing proportion for each lineup member
#What about cases in which a member wasn't chosen at all? Data should reflect this - how?
#What about cases in which mock witness chose no one?
allprop <- function(linevec, susp_pos){
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(susp_pos)))
    for (i in 1:length(susp_pos)){
        propvec[i,]=lineup_prop_pos_vec(linevec, susp_pos[i])
    }
    return(propvec)
}