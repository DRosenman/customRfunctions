library(dplyr)
setexclusive <- function(x,y){
  # x is the first dataframe
  # y is the second dataframe
  
  #Consists of rows in dataframe x or dataframe y but not both dataframes x and y.
  
  x %>% dplyr::mutate_if(is.factor,as.character) -> x
  y %>% dplyr::mutate_if(is.factor,as.character) -> y
  union_df <- dplyr::union(x,y)
  common_df <- dplyr::intersect(x,y)
  return(dplyr::setdiff(union_df,common_df))
  
}