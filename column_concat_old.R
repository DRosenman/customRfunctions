#Function for combining two character columns (seperating the character strings by "," by default).
column_concat <- function(df,col1,col2,colname, sep = ",") {
  #df is the dataframe
  #col1 is the name of the first column to be combined
  #col2 is the name of the second column to be combined
  #colname is the name of the output column
  #sep is the character that will separate the combined column strings
  
  #returns a dataframe with the first column, 'colname', being a combination of two columns seperated by "sep"
  #If the values in each column for a row are NA, the output for that row in 'colname' will be NA. 
  # If one but not both of the values of the combined columns in a row is NA, NA will be removed
  #
  #example:
  #df <- data.frame(x = c("U1,U2,U3","U1,U2","NA","NA"),y = c("V4,V5","NA","V8","NA"),stringsAsFactors = FALSE)
  #print(df)
  #        x     y
  #  1 U1,U2,U3 V4,V5
  #  2    U1,U2    NA
  #  3       NA    V8
  #  4       NA    NA
  #column_concat(df,x,y,z)
  #        z        x     y
  # 1 U1,U2,U3,V4,V5 U1,U2,U3 V4,V5
  # 2          U1,U2    U1,U2    NA
  # 3             V8       NA    V8
  # 4           <NA>       NA    NA
  
  c1 <- enquo(col1)
  c2 <- enquo(col2)
  col <- enquo(colname)
  df <- df %>% unite(!!col,!!c1,!!c2,sep = sep,remove = FALSE)
  df[,1] <- str_replace_all(df[,1],c("NA,NA" = NA, "NA," = "", ",NA" = ""))
  
  return(df)
}
