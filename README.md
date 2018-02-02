Useful Functions
================
Dave Rosenman
February 2, 2018

What follows is a demonstration of some R functions that I made that (I think) are useful.

Combining Columns
-----------------

The following function, **column\_concat** let's you combine two columns of srins into a single column. It returns the original columns from the dataframe plus the combined column.

### parameters

-   df is the dataframe
-   col1 is the name of the first column to be combined
-   col2 is the name of the second column to be combined
-   colname is the name of the output column
-   sep is the character that will separate the combined column strings

### returns

A dataframe with the first column, 'colname', being a combination of two columns seperated by "sep". If the values in each column for a row are NA, the output for that row in 'colname' will be NA. If one but not both of the values of the combined columns in a row is NA, NA will be removed

### required packages

-   **tidyr**: for the *unite* function
-   **rlang**: for the *enquo* function
-   **stringr** for the *string\_replace\_all* function

``` r
library(tidyr)
library(rlang)
library(stringr)

column_concat <- function(df,col1,col2,colname, sep = ",") {
  c1 <- enquo(col1)
  c2 <- enquo(col2)
  col <- enquo(colname)
  df <- df %>% tidyr::unite(!!col,!!c1,!!c2,sep = sep,remove = FALSE)
  df[,1] <- str_replace_all(df[,1],c("NA,NA" = NA, "NA," = "", ",NA" = ""))
  return(df)
}
```

**Example**:

``` r
df <- data.frame(x = c("U1,U2,U3","U1,U2","NA","NA"),
                 y = c("V4,V5","NA","V8","NA"), 
                 z = 9,
                 stringsAsFactors = FALSE)
print(df)
```

    ##          x     y z
    ## 1 U1,U2,U3 V4,V5 9
    ## 2    U1,U2    NA 9
    ## 3       NA    V8 9
    ## 4       NA    NA 9

``` r
df_new <- column_concat(df,x,y,xy)
print(df_new)
```

    ##               xy        x     y z
    ## 1 U1,U2,U3,V4,V5 U1,U2,U3 V4,V5 9
    ## 2          U1,U2    U1,U2    NA 9
    ## 3             V8       NA    V8 9
    ## 4           <NA>       NA    NA 9
