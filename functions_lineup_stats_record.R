# Functions for lineup statistics package

library(pacman)
p_load(tidyverse, magrittr)

# Function for taking a vector representing frequencies
# of choices per foil (ie a 1d array), for varying lineups sizes, and creating
# a vector of individual choices of lineup members.  Lineup size has to 
# be declared explicitly, since there could be zero foil choices

# First make a function that repeats a number k times
rep_index <- function(index,num){
    rep(index, num)
}

gen_linevec_table <- function (line_table, nom_size){
    line_index <- 1:(length(line_table))
    map2(line_index, line_table, rep_index) %>% unlist   
}

# Function for generating bootstrap samples from a lineup vector
# created with gen_linevec_table or from a raw vector. Number of
# bootstrap samples is controlled by argument bootno
gen_boot_samples <- function (linevec, bootno){
    names_a <- rep("sample_",bootno)
    names_b <- as.character(1:bootno)
    bootno %>% rerun(sample(linevec, length(linevec), replace = TRUE))  -> x 
    names(x) <- paste(names_a,names_b,sep = "")
    lineup_boot_samples <- map_df(x, extract, c(1:length(linevec)))
}
 
# names_a <- rep("sample_",100)
# names_b <- as.character(1:100)
# 100 %>% rerun(sample(avec, length(avec), replace = TRUE))  -> x 
# names(x) <- names_a
# names(x) <- paste(names_a,names_b,sep = "")
# z <- map_df(x, extract, c(1:7))

# Function for computing lineup prop from a lineup table
# The function needs to have the lineup size and member position
# declared; assumes a vector as input
lineup_prop_pos <- function (lineup_table,susp_pos){
    # lineup_table = table(lineup_vec)
    lineup_table[susp_pos]/sum(lineup_table)   
}
    
# Function for computing lineup prop over a df
# usualy a bootstrapped set
gen_lineup_prop <- function (lineup_boot_df, susp_pos){
    map(lineup_boot_df,~table(.)) %>% 
    map_dbl(., ~ lineup_prop_pos(.,susp_pos))
}

# Function for computing mean. med and se of boot proportion
gen_boot_propmean_se <- function (lineuprops){
  mean_boot_prop = mean(lineuprops, na.rm = T)
  median_boot_prop = median(lineuprops, na.rm = T)
  stdev_boot_prop = sd(lineuprops, na.rm = T)
  n = length(lineuprops)
  std_error_boot_prop    = stdev_boot_prop/sqrt(n)
  ci025 = gen_boot_propci(lineuprops,.025)
  ci975 = gen_boot_propci(lineuprops,.975)
cat("Boot prop. (mean)   = ", mean_boot_prop,"\n")
cat("Boot prop. (median) = ", median_boot_prop,"\n")
cat("SD of boot prop     = ", stdev_boot_prop,"\n")
cat("SE of boot prop     = ", stdev_boot_prop/sqrt(n),"\n")
cat("2.5% boot CI lvl    = ", ci025,"\n")
cat("97.5% boot CI lvl   = ", ci975,"\n")
}

# Function for computing arbitrary percentile of boot proportion
# assuming a bootstrap sample df, and perc as argument
gen_boot_propci <- function (lineuprops, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineuprops, probs = perc)
    }
    else {
        cat("Illegal value entered (must be proportion)")
    }
}


# Function for generating I component of Effective Size (Tredoux, 1998)
i_T <- function(linetable){
    i <- 1-(1/(sum(linetable)^2))*sum(linetable^2) 
} 

# Function for generating Effective Size (Tredoux, 1998)
esize_T <- function(linetable){
    i <- 1-(1/(sum(linetable)^2))*sum(linetable^2) 
    i <- 1/(1-i)
} 

# Function for computing Effective Size (Tredoux, 1998) on
# lineups contained as columns in a df, usually from bootstrap
gen_esize_T <- function (lineup_boot_df){
    map(lineup_boot_df,~table(.)) %>% 
        map_dbl(., ~ esize_T(.))
}

# Function for computing Effective Size (Tredoux, 1998)
# With CIs from normal theory
esize_T_ci_n <- function(linetable, alpha){
    N = sum(linetable)
    a <- 4/N
    b <- sum((linetable/N)^3)
    c <- (sum((linetable/N)^2)^2)
    var_i <- a*(b-c)
    sd_i <- sqrt(var_i)
    ci_low <- i_T(linetable) + qnorm((1-alpha)/2)*sd_i
    ci_high <- i_T(linetable) + qnorm(alpha+((1-alpha)/2))*sd_i
    e_ci <- list(ci_low = 1/(1-ci_low), ci_high = 1/(1-ci_high))
}

# Function for computing Effective Size (Tredoux, 1998)
# With CIs from bootstrap df of lineups
gen_esize_T_ci <- function (lineupesizes, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineupesizes, probs = perc)
    }
    else {
        cat("Illegal value entered (perc must be proportion)")
    }
}


# Function for computing Effective Size (Malpass, 1981)
esize_m <- function (linetable, printarg=FALSE){
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

# Function for computing ES (Malpass, 1981) over a df
# usually from bootstrap replications of lineup
gen_esize_m <- function (lineup_boot_df){
    map(lineup_boot_df,~table(.)) %>% 
        map_dbl(., ~ esize_m(.))
}

# Function for computing Effective Size (Malpass, 1981, 
# as adjusted by Tredoux, 1998
# With CIs from bootstrap df of lineups
gen_esize_m_ci <- function (lineupesizes, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineupesizes, probs = perc)
    }
    else {
        cat("Illegal value entered (perc must be proportion)")
    }
}


# Function for computing Effective Size (Malpass, 1981)
# counting foils who fall within the CI for chance guessing
esize_m_f <- function (linetable){
   
    
    
}
# Helper function to rotate vector
rot_vector <- function(avec){
    avec1 <- lag(avec)
    avec1[1] <- avec[length(avec)]
    avec <- avec1
}

# Bunch of functions that compute bootcis from
# proportions
makevec_prop <- function(prop,n){
    avec_1 = rep(1,n*prop)
    avec_2 = rep(0,n-n*prop)
    avec = c(avec_1,avec_2)
}

bp <- function(vec){
    (sum(sample(vec, length(vec),replace = TRUE) == TRUE))/length(vec) 
}

boot025 <- function(prop,n){
    y = makevec_prop(prop,n)
    x <- map_dbl(1:1000,~bp(y))
    quantile(x,probs = .025)
}

boot0975 <- function(prop,n){
    y = makevec_prop(prop,n)
    x <- map_dbl(1:1000,~bp(y))
    quantile(x,probs = .975)
}


# Function to compute bias for each lineup member (assuming foil
# is suspect, from Malpass, 1981)
allfoilbias <- function (linetab){
    # Make df for testing
    linedf <- as.data.frame(matrix(ncol = length(linetab), 
                                   nrow = length(linetab)))
    linebias <- NULL
    for (i in 1:length(linetab)){
        linebias[i] = lineup_prop_pos(linetab,5)
        linedf[,i] = rot_vector(linetab)
        linetab = linedf[,i]
    }
    # linebias[length(linetab)] = lineup_prop_pos(linetab,1)
    linebias
}

x <- (allfoilbias(ltab))

# Function to compute cihigh for each foil in a lineup
# see Malpass, 1981
allfoil_cihigh <- function(linetabprops, sumlineup){
z <- 1:length(linetabprops)
for (i in 1:length(linetabprops)){
   z[i] <-  boot0975(linetabprops[i],sumlineup)
}
z
}

allfoil_cilow <- function(linetabprops, sumlineup){
    z <- 1:length(linetabprops)
    for (i in 1:length(linetabprops)){
        z[i] <-  boot025(linetabprops[i],sumlineup)
    }
    z
}

zx <- allfoil_cihigh(x, sum(ltab))
px <- allfoil_cilow(x, sum(ltab))
zx
px

lapply(x,boot0975)
map2(x,y,~boot0975())
boot0975(.2,sum(ltab))
# Functions to come========

# Function for computing Effective Size (Malpass, 1981)
# counting foils who fall within the CI for chance guessing
*Get bootstrapping figured out


# Function for computing ES (Malpass, 1981) over a df
# counting foils who fall within the CI for chance guessing

*same as above?


# Function for computing bootstrap CIS for ES (Malpass, 1981) 
# counting foils who fall within the CI for chance guessing
*Bootstrapping issue
In this fx, you are bootstrapping the one above
output will be es, ci low and ci high




# Function for computing bias CIS for each foil (Malpass, 1981) 
+

*Done thuis in first fx above

# Function for computing diagnosticity of a lineup pair (Tredoux, 1998)


*Done



# Function for comparing diagnosticity of two indep lineups (Tredoux, 1998)

*Done? How is this different?



# Function for comparing two effective sizes (Tredoux, 1998)
*Still to do
find CI around each effective size
do the CIs overlap?
take diff between 2 es and see whether 0 falls w/in that CI
get 2 es
take e1-e2
if 0 doesn't fall w/in interval then significantly different


# Function for computing Functional Size (Wells, 1979)
# with both normal theory CI and bootstrap CI

*Done - see Tredoux fx





# Master function for assessing bias of a lineup
*Still need to do




# Master function for assessing size of a lineup

*Still need to do


# Master function for assessing bias and size of a lineup


*Still need to do


# Experimental function for assessing lineup bias and size
# from frontal photographs of the lineup
*Still to do





# Function for computing the ROC for an SDT based lineup study
# conditional on confidence

*Still to do




# Trial code ==========
ltab <- c(1,2,29,2,2,5)
lvec <- gen_linevec_table(ltab)
boot_try <- gen_boot_samples(lvec, 1000)
esize_m(ltab,printarg=TRUE)
e_m <- gen_esize_m(boot_try)
gen_esize_m_ci(e_m,.025)
gen_esize_m_ci(e_m,.975)
(esize_T_ci_n(ltab,.95))
(esize_T(ltab))
# Workflow
# Given vector


# Given table
vvec <- round(runif(100,0,6))
# Have to strip illegal nums out
vvec <- vvec[vvec %in% 1:6]
# Make table
ltab <- table(vvec)
ltab[3]/sum(ltab)
btab <- c(5,5,15,6,6,8,6)

# generate bootstrap sample
lvec <- gen_linevec_table(btab)
boot_try <- gen_boot_samples(lvec, 1000)
bootprops <- gen_lineup_prop(boot_try, 3)
gen_boot_propmean_se(bootprops)
