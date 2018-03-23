#Base function for getting bootstrapped df of lineup proportion
#Used to compute CIs
#Calls function for computing lineup prop from vector

lineup_prop_pos_boot <- function (linevec, d=d ,susp_pos){
    return (lineup_prop_pos(linevec[d]), susp_pos)
}