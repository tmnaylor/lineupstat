<<<<<<< HEAD
#Tamsyn's Functions

#Function for computing lineup prop from a vector of lineup choices
# The function needs to have the member position declared
#Takes vector as input

lineup_prop_vec <- function(linevec, susp_pos){
    sum(linevec == susp_pos)/length(linevec)
}

#Base function for computing bootstrapped lineup proportion for 1 lineup member
#Takes vector of lineup choices and member position (single number) as inputs
#E.g. boot(linevec, lineup_prop_pos_boot, 2, R = 1000)
#Needs to be labelled more clearly
lineup_prop_boot <- function(linevec, d=d,  susp_pos){
    sum(linevec[d] == susp_pos)/length(linevec)
}

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

#Alternative way of writing function above
allprop <- function(linevec){
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(susp_pos)))
    susp_pos <- sort(unique(linevec))
    for (i in 1:length(susp_pos)){
        propvec[i,]=lineup_prop_pos_vec(linevec, susp_pos[i])
    }
    return(propvec)
}

#Function for computing bootstrapped cis of proportion for each lineup member
#Inputs - vector of lineup choices, vector of lineup positions
lineup_boot_allprop <- function(avec, pos){
    z <- map(pos,~boot(avec, lineup_prop_boot, susp_pos = .x, R = 1000) %>% 
                 boot.ci(type = "bca")) %>% 
        map(extract, "bca") %>% 
        map_df(extract,"bca")
    
    z2 <-  matrix(ncol = 6,nrow = 5, z$bca) %>% 
        data.frame() %>% 
        slice(4:5)
    ci <- as.data.frame(t(ci))
    
    return(tci)
}

# Function for computing Effective Size (Malpass, 1981)
# counting foils who fall within the CI for chance guessing
es_foil_count <- function(linevec, susp_pos){
    ci <- lineup_boot_allprop(linevec, susp_pos)
    k <- 1/length(susp_pos)
    ci_count <- cbind(ci[,1] <= k & ci[,2] >= k)
    print(sum(ci_count == TRUE))
}

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

# Tredoux diagnosticity ratio
# This function computes diagnosticity ratio from raw data -- allows user to do this for one set pf ta/tp lineups
# lineup_pres = vector of choices for single target present lineup
#lineup_abs = vector of choices for single target absent lineup
#pos_pres = suspect position in target present lineup (direct input by user)
#pos_abs = suspect position in target absent lineup (direct input by user)
#position of lineups to be compared must correspond across ta/tp vectors

diag_ratio_T <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- (sum(lineup_pres == pos_pres) + 0.5)/(length(lineup_pres) + 0.5)
    b <- (sum(lineup_abs == pos_abs) + 0.5)/(length(lineup_abs) + 0.5)
    c <- a/b
} 

# Variance of Tredoux diagnosticity ratio
# Let's either define what x and t are explicitly or
# rename them so that their names are easier to grasp
# e.g. suspect_pos_tp and suspect_pos_ta
var_diag_ratio <- function(vec1, vec2, x, t){
    a <- sum(vec1  != x)
    b <- (sum(vec1 == x))*(t+1)
    c <- sum(vec2  != x)
    d <- (sum(vec2 == x))*(t+2)
    e <- (a/b)+(c/d)
}

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

# Computing ln(d) for k lineups
# Needs df of diagnosticity parameters --> get using helper fx
# OK, but let's say what the helper fx is and what it computes

ln_diag_ratio <- function(df){
    d   <- (df$n11+0.5/((df$n11+df$n21)+0.5))/
        (df$n12+0.5/((df$n12+df$n22)+0.5))
    lnd <- log(d)
    lnd <- as.data.frame(lnd)
    
    return(lnd)
}


# Function to compute variance of ln(d) for k lineups
# Needs helper fx, state what it is, what it computes etc.

var_lnd <- function(df){
    var <- (df$n21/(df$n11+(df$n11+df$n21)))+(df$n22/(df$n12+(df$n12+df$n22)))
    var <- as.data.frame(var)
    return(var)
}

#Function to compute weights for diagnosticity ratios in k lineup pairs

d_weights <- function(df){
    numerator   <- df$n11*df$n12*(df$n11+df$n21)*(df$n12+df$n22)
    denominator <- df$n11*df$n22*(df$n11+df$n21)+
        df$n12*df$n21*(df$n12+df$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)
    
}

# Function for computing pooled estimator from a set of k diagnosticity ratios
# df must be a dataframe combining vectors of requisite paramater estimates 
# (created in prev steps)

d_bar <- function(df){
    numerator   <- sum(df$wi*df$lnd)
    denominator <- sum(df$wi)
    d_bar       <- exp(numerator/denominator)
    return(d_bar)
}

#Function for getting chi-squared value for homogeneity of diagnosticity ratios

chi_diag <- function(df, d_bar){
    q <- sum(((df$lnd-log(d_bar))^2)/(df$var))
    return(q)
}

#Base function for computing bootstrapped effective size
esize_m_boot <- function (linevec, d, printarg=FALSE){
    linetable <- table(linevec[d])
    k <- length(linetable)
    ea <- sum(linetable)/k
    x <- sum(abs(linetable-ea)/(2*ea))
    esize_ma = k-x
    ka <- sum(linetable!=0)
    linetable_a <- linetable[linetable!=0]
    ea <- (sum(linetable))/ka
    xa <- sum(abs(linetable_a-ea)/(2*ea))
    esize_ma_a = ka-xa
    if (printarg) {
        cat("Effective size (Malpass, 1981) = ", esize_ma_a,"\n")
        cat("Effective size (Malpass, 1981,","\n", 
            "            adj Tredoux, 1998) = ", esize_ma, "\n")
    }
    esize_ma
}  
=======
#Tamsyn's Functions

#Function for computing lineup prop from a vector of lineup choices
# The function needs to have the member position declared
#Takes vector as input

lineup_prop_vec <- function(linevec, susp_pos){
    sum(linevec == susp_pos)/length(linevec)
}

#Base function for computing bootstrapped lineup proportion for 1 lineup member
#Takes vector of lineup choices and member position (single number) as inputs
#E.g. boot(linevec, lineup_prop_pos_boot, 2, R = 1000)
#Needs to be labelled more clearly
lineup_prop_boot <- function(linevec, d=d,  susp_pos){
    sum(linevec[d] == susp_pos)/length(linevec)
}

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

#Alternative way of writing function above
allprop <- function(linevec){
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(susp_pos)))
    susp_pos <- sort(unique(linevec))
    for (i in 1:length(susp_pos)){
        propvec[i,]=lineup_prop_pos_vec(linevec, susp_pos[i])
    }
    return(propvec)
}

#Function for computing bootstrapped cis of proportion for each lineup member
#Inputs - vector of lineup choices, vector of lineup positions
lineup_boot_allprop <- function(avec, pos){
    z <- map(pos,~boot(avec, lineup_prop_boot, susp_pos = .x, R = 1000) %>% 
                 boot.ci(type = "bca")) %>% 
        map(magrittr::extract, "bca") %>% 
        map_df(magrittr::extract,"bca")
    
    z2 <-  matrix(ncol = 6,nrow = 5, z$bca) %>% 
        data.frame() %>% 
        slice(4:5)
    ci <- as.data.frame(t(z2))
    
    return(ci)
}

# Function for computing Effective Size (Malpass, 1981)
# counting foils who fall within the CI for chance guessing
es_foil_count <- function(linevec, susp_pos){
    ci <- lineup_boot_allprop(linevec, susp_pos)
    k <- 1/length(susp_pos)
    ci_count <- cbind(ci[,1] <= k & ci[,2] >= k)
    print(sum(ci_count == TRUE))
}

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

# Tredoux diagnosticity ratio
# This function computes diagnosticity ratio from raw data -- allows user to do this for one set pf ta/tp lineups
# lineup_pres = vector of choices for single target present lineup
#lineup_abs = vector of choices for single target absent lineup
#pos_pres = suspect position in target present lineup (direct input by user)
#pos_abs = suspect position in target absent lineup (direct input by user)
#position of lineups to be compared must correspond across ta/tp vectors

diag_ratio_T <- function(lineup_pres, lineup_abs, pos_pres, pos_abs){
    a <- (sum(lineup_pres == pos_pres) + 0.5)/(length(lineup_pres) + 0.5)
    b <- (sum(lineup_abs == pos_abs) + 0.5)/(length(lineup_abs) + 0.5)
    c <- a/b
} 

# Variance of Tredoux diagnosticity ratio
# Let's either define what x and t are explicitly or
# rename them so that their names are easier to grasp
# e.g. suspect_pos_tp and suspect_pos_ta
var_diag_ratio <- function(vec1, vec2, x, t){
    a <- sum(vec1  != x)
    b <- (sum(vec1 == x))*(t+1)
    c <- sum(vec2  != x)
    d <- (sum(vec2 == x))*(t+2)
    e <- (a/b)+(c/d)
}

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

# Computing ln(d) for k lineups
# Needs df of diagnosticity parameters --> get using helper fx
# OK, but let's say what the helper fx is and what it computes

ln_diag_ratio <- function(df){
    d   <- (df$n11+0.5/((df$n11+df$n21)+0.5))/
        (df$n12+0.5/((df$n12+df$n22)+0.5))
    lnd <- log(d)
    lnd <- as.data.frame(lnd)
    
    return(lnd)
}


# Function to compute variance of ln(d) for k lineups
# Needs helper fx, state what it is, what it computes etc.

var_lnd <- function(df){
    var <- (df$n21/(df$n11+(df$n11+df$n21)))+(df$n22/(df$n12+(df$n12+df$n22)))
    var <- as.data.frame(var)
    return(var)
}

#Function to compute weights for diagnosticity ratios in k lineup pairs

d_weights <- function(df){
    numerator   <- df$n11*df$n12*(df$n11+df$n21)*(df$n12+df$n22)
    denominator <- df$n11*df$n22*(df$n11+df$n21)+
        df$n12*df$n21*(df$n12+df$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)
    
}

# Function for computing pooled estimator from a set of k diagnosticity ratios
# df must be a dataframe combining vectors of requisite paramater estimates 
# (created in prev steps)

d_bar <- function(df){
    numerator   <- sum(df$wi*df$lnd)
    denominator <- sum(df$wi)
    d_bar       <- exp(numerator/denominator)
    return(d_bar)
}

#Function for getting chi-squared value for homogeneity of diagnosticity ratios

chi_diag <- function(df, d_bar){
    q <- sum(((df$lnd-log(d_bar))^2)/(df$var))
    return(q)
}

#Base function for computing bootstrapped effective size

esize_m_boot <- function (linevec, d, printarg=FALSE){
    linetable <- table(linevec[d])
    k <- length(linetable)
    ea <- sum(linetable)/k
    x <- sum(abs(linetable-ea)/(2*ea))
    esize_ma = k-x
    ka <- sum(linetable!=0)
    linetable_a <- linetable[linetable!=0]
    ea <- (sum(linetable))/ka
    xa <- sum(abs(linetable_a-ea)/(2*ea))
    esize_ma_a = ka-xa
    if (printarg) {
        cat("Effective size (Malpass, 1981) = ", esize_ma_a,"\n")
        cat("Effective size (Malpass, 1981,","\n", 
            "            adj Tredoux, 1998) = ", esize_ma, "\n")
    }
    esize_ma
}  
>>>>>>> 560a177881d2672116fd2771b1a6d65a6111fc1a
