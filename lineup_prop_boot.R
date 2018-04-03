#Base function for computing bootstrapped lineup proportion for 1 lineup member
#Takes vector of lineup choices and member position (single number) as inputs
#E.g. boot(linevec, lineup_prop_pos_boot, 2, R = 1000)
#Needs to be labelled more clearly
lineup_prop_pos_boot <- function(linevec, d=d,  susp_pos){
    sum(linevec[d] == susp_pos)/length(linevec)
}
