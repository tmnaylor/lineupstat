#Base function for computing proportion for each lineup member
#Using package boot
#Takes vector of lineup choices and vector of lineup member positions as inputs
#Doesn't work yet
allprop_boot <- function(linevec, d=d, susp_pos){
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(susp_pos)))
    for (i in 1:length(susp_pos)){
        propvec[i,]=lineup_prop_pos_vec(linevec[d], susp_pos[i])
    }
    return(propvec)
} 