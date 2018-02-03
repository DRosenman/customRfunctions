Useful Functions
================
Dave Rosenman
February 2, 2018

What follows is a demonstration of some R functions that I made that (I
think) are useful.

## paste\_columns

**paste\_columns** lets you combines multiple columns of strings into a
single column. It returns the original columns from the dataframe plus
the combined columns.

### Parameters

  - df is the dataframe
  - … the column names of the columns that will be combined.
  - colname is the name of the output column
  - sep is the character that will separate the combined column strings

### Returns

A dataframe with the first column, “col”, being a combination of the
column … separated by “sep” (default “,”). Note: If the values in each
column for a row are NA, the output for that row in ‘col’ will be NA. If
one or more, but not all, values of the combined columns in a row are
NA, all NA’s will be remove in ‘col’ for that row.

### Required Packages

  - **tidyr**: for the *unite* function
  - **rlang**: for the *enquo* function
  - **stringr** for the *string\_replace\_all* function

<!-- end list -->

``` r
library(tidyr)
library(rlang)
library(stringr)

paste_columns <- function(df,...,col, sep = ",") {
  cols <- quos(...)
  col <- enquo(colname)
  df <- df %>% unite(!!col,!!!cols,sep = sep,remove = FALSE)
  n <- length(cols) 
  na_string <- paste(replicate(n,"NA"), collapse = ",")
  df[,1] <- str_replace_all(df[,1],c(na_string = NA, "NA," = "", ",NA" = ""))
  
  return(df)
}
```

### Example

``` r
df <- data.frame(x = c("U1,U2,U3","U1,U2",NA,NA),
                 y = c("V4,V5",NA,"V8",NA), 
                 z = c('A',NA,'C',NA),
                 stringsAsFactors = FALSE)
print(df)
```

    ##          x     y    z
    ## 1 U1,U2,U3 V4,V5    A
    ## 2    U1,U2  <NA> <NA>
    ## 3     <NA>    V8    C
    ## 4     <NA>  <NA> <NA>

``` r
df_new <- paste_columns(df,x,y,z,col = xyz)
print(df_new)
```

    ##   function (expr) ...        x     y    z
    ## 1    U1,U2,U3,V4,V5,A U1,U2,U3 V4,V5    A
    ## 2               U1,U2    U1,U2  <NA> <NA>
    ## 3                V8,C     <NA>    V8    C
    ## 4                  NA     <NA>  <NA> <NA>

### setexclusive

dplyr contains there set operations functions. All three work on
complete rows by comparing the values of each variable a given row. The
two input dataframes need to have the same variables. *intersect*
returns equivalent rows from the two dataframes, *union* returns unique
rows in the two dataframes, and *setdiff* returns rows that are in the
first dataframe but not the second. **setexclusive** returns rows that
are in either the first dataframe or the second dataframe, but not both
dataframes.

### Parameters

  - x is the first dataframe
  - y is the second dataframe

### Returns

A dataframe consisting of rows in x that are not in dataframe y and rows
in y that are not in dataframe x.

### Required Package

  - **dplyr**

<!-- end list -->

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
setexclusive <- function(x,y) {
  x %>% mutate_if(is.factor,as.character) -> x
  y %>% mutate_if(is.factor,as.character) -> y
  union_df <- union(x,y)
  common_df <- intersect(x,y)
  return(setdiff(union_df,common_df))
  
}
```

### Example

``` r
library(tibble)
```

    ## 
    ## Attaching package: 'tibble'

    ## The following object is masked from 'package:rlang':
    ## 
    ##     has_name

``` r
df1 <- tribble(
      ~x,~y,
       1,2,
       2,3,
       3,4,
       4,5
      )
df2 <- tribble(
      ~x,~y,
       1,2,
       9,10,
       2,3
)
#df1 and df2 both have rows (x = 1,y = 2) and (x = 2, y = 3)
#rows exclusive to df1 are rows (x = 3,y = 4) and (x = 4,y = 5)
#the only row exclusive to df2 is (x = 9, y = 10).


setexclusive(df1,df2)
```

    ## # A tibble: 3 x 2
    ##       x     y
    ##   <dbl> <dbl>
    ## 1     9    10
    ## 2     3     4
    ## 3     4     5
