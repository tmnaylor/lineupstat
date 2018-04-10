library(boot);library(tidyverse)

lineup_prop_pos_vec <- function(linevec, susp_pos){
    sum(linevec == susp_pos)/length(linevec)
}


# This function takes a vector of lineup choices and for each member, calculates proportion:
  
    allprop <- function(linevec, susp_pos){
        propvec <- as.data.frame(matrix(ncol= 1,
                                        nrow = length(susp_pos)))
        for (i in 1:length(susp_pos)){
            propvec[i,]=lineup_prop_pos_vec(linevec, susp_pos[i])
        }
        return(propvec)
    }



lineup_prop_b <- function(linevec, d, susp_pos){
    sum(linevec[d] == susp_pos)/length(linevec)
}

xfun <- function(anyvec, susp_pos,...){
         boot(anyvec, lineup_prop_b, susp_pos, R = 100) %>% 
         boot.ci(type = "bca")
}

xfun(avec,susp_pos = 1)

boot(avec, lineup_prop_b, susp_pos = 1, R = 100) %>% 
             boot.ci(type = "bca")

map(pos,~boot(avec, lineup_prop_b, susp_pos = .x, R = 100) %>% 
    boot.ci(type = "bca") 
        

y <- boot.ci(x, type = "bca")
y$bca[4:5]
boot.ci(x, type = "bca")$bca[4:5]


