# Wells diagnosticity ratio
#lineup_pres = vector of choices for single target present lineup
#lineup_abs = vector of choices for single target absent lineup
#pos_pres = suspect position in target present lineup (direct input by user)
#pos_abs = suspect position in target absent lineup (direct input by user)
#position of lineups to be compared must correspond across ta/tp vectors

diag_ratio_W <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- sum(lineup_pres == pos_pres)/(length(lineup_pres))
    b <- sum(lineup_abs == pos_abs)/(length(lineup_pres))
    c <- a/b
}