#'Bootstrap resampling
#'
#'Function for generating bootstrapped samples of lineup data from 1 vector
#'@param lineup_vec A numeric vectors of lineup choices
#'@param bootno Number of bootstrap samples
#'@examples
#'names_a <- rep("sample_",100)
#'names_b <- as.character(1:100)
#'100 %>% rerun(sample(avec, length(avec), replace = TRUE))  -> x
#'names(x) <- names_a
#'names(x) <- paste(names_a,names_b,sep = "")
#'z <- map_df(x, magrittr:extract, c(1:7))

gen_boot_samples <- function (lineup_vec, bootno){
  names_a <- rep("sample_",bootno)
  names_b <- as.character(1:bootno)
  bootno %>% rerun(sample(lineup_vec, length(lineup_vec), replace = TRUE))  -> x
  names(x) <- paste(names_a,names_b,sep = "")
  lineup_boot_samples <- map_df(x, magrittr::extract, c(1:length(lineup_vec)))
}

