#'Bootstrapped sampling
#'
#'Function for generating bootstrapped samples for of k vectors of lineup choices
#'@param lineup_list A list containing k vectors of lineup choices for k lineups,
#'                        in which the target was either absent or present
#'@param bootno Number of bootstrap samples
#'@returns A list of bootstrapped data
#'         Length of list = No. of boostrap sample draws
#'@example
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(100,1,6))
#'C <-  round(runif(100,1,6))
#'df <- cbind(A, B, C)
#'df <- as.data.frame(df)
#'lineup_list <- as.list(df)
#'bootno <- 1000
#'gen_boot_samples(linuep_list, bootno)

gen_boot_samples_list <- function (lineup_list, bootno){
  lineup_boot_samples <- NULL
  for (i in 1:length(lineup_list)){

    names_a <- rep("sample_",bootno)
    names_b <- as.character(1:bootno)
    bootno %>% rerun(sample(lineup_list[[i]], length(lineup_list[[i]]), replace = TRUE))  -> x
    names(x) <- paste(names_a,names_b,sep = "")
    lineup_boot_samples[[i]] <- map_df(x, magrittr::extract, c(1:length(lineup_list[[i]])))
  }
  return(lineup_boot_samples)
}
