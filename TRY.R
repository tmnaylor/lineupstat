library(boot);library(tidyverse);library(magrittr)


lineup_prop_b <- function(linevec, d, susp_pos){
    sum(linevec[d] == susp_pos)/length(linevec)
}

lineup_boot_allprop <- function(avec, pos){
    z <- map(pos,~boot(avec, lineup_prop_b, susp_pos = .x, R = 100) %>% 
        boot.ci(type = "bca")) %>% 
        map(extract, "bca") %>% 
        map_df(extract,"bca")
    
    z2 <-  matrix(ncol = 6,nrow = 5, z$bca) %>% 
            data.frame() %>% 
            slice(4:5)
    return(z2)
}
        
        
anvec <- round(runif(100,1,6))
apos <- 1:6

(lineup_boot_allprop(anvec,apos))

