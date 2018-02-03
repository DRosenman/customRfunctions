library(tidyr)
library(rlang)
library(stringr)

paste_columns <- function(df,...,col, sep = ",") {
  #df is the dataframe
  #... the columns you want to combine
  #col is the name of the output column
  #sep is the character that will separate the combined column strings
  
  #returns:
  # - A dataframe with the first column, col , being a combination of the columns ... separated by "sep" (default ",").
  # - Note: If the values in each column for a row are NA, the output for that row in 'col' will be NA.
  #         If one or more, but not all, values of the combined columns in a row are NA, all NA's will be remove in 'col' for that row.   
  
  
  #example:
  #df <- data.frame(x = c("U1,U2,U3","U1,U2",NA,NA),
  #                 y = c("V4,V5",NA,"V8",NA), 
  #                 z = c('A',NA,'C',NA),
  #                 stringsAsFactors = FALSE)
  #> df
  #   x            y       z
  # 1 U1,U2,U3   V4,V5     A
  # 2 U1,U2       <NA>    <NA>
  # 3 <NA>         V8      C
  # 4 <NA>        <NA>   <NA>
  
  #> paste_columns(df,x,y,z,col = xyz)
  #       xyz               x          y      z
  # 1 U1,U2,U3,V4,V5,A    U1,U2,U3     V4,V5    A
  # 2     U1,U2           U1,U2        <NA>   <NA>
  # 3      V8,C           <NA>         V8     C
  # 4     <NA>            <NA>        <NA>   <NA>
  
  cols <- quos(...)
  col <- enquo(col)
  df <- df %>% unite(!!col,!!!cols,sep = sep,remove = FALSE)
  n <- length(cols) 
  na_string <- paste(replicate(n,"NA"), collapse = ",")
  df[,1] <- str_replace_all(df[,1],c(na_string = NA, "NA," = "", ",NA" = ""))
  replace(df,df == "NA",NA)
}
