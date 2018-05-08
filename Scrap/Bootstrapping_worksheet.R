---
title: "Bootstrapping with slipper"
output: html_notebook
---
```{r}
mtcars %>% slipper(mean(mpg),B=100) %>%
  filter(type=="bootstrap") %>% 
  summarize(ci_low = quantile(value,0.025),
            ci_high = quantile(value,0.975))
```

```{r}
#Making test df1 (present)

A <-  round(runif(100,1,6)) 

B <-  round(runif(20,1,6)) 

C <-  round(runif(100,1,6)) 

D <-  round(runif(40,1,2)) 

E <-  round(runif(55,1,6))  

F <-  round(runif(100,1,4)) 

list1 <- list(A, B, C, D, E, F)
rm(A, B, C, D, E, F)

#Making vector of suspect positions for list1
pos1 <- c(1, 2, 3, 2, 5, 2)

#Making test df2 (absent)

A <-  round(runif(60,1,6)) #Present 

B <-  round(runif(100,1,6)) #Absent 

C <-  round(runif(89,1,4)) #Present 

D <-  round(runif(22,1,3)) #Absent

E <-  round(runif(100,1,6)) #Present 

F <-  round(runif(100,1,2)) #Absent

list2 <- list(A, B, C, D, E, F)
rm(A, B, C, D, E, F)

#Making vector of suspect positions for list2
pos2 <- c(1, 2, 3, 4, 5, 6)
```

```{r}
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
        
    }
    return(diagdf)
}

linedf <- diag_param(list1, list2, pos1, pos2)
```


#Bootstrap DI
```{r}
slipper()


```
#Boostrap d_bar

#Bootstrap p-value (chi)
