#Function for computing lineup prop from a vector of lineup choices
# The function needs to have the member position declared

lineup_prop_pos_vec <- function(linevec, susp_pos){
    sum(linevec == susp_pos)/length(linevec)
}