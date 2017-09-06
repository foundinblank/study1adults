a <- 1:4
b <- 5:8
c <- 9:12
d <- 13:16
e <- 17:20
f <- 21:24
df <- data.frame(a,b,c,d,e,f)

# Function to select a few columns using colon operator
foo <- function(df,x,y,z,a) {
  x <- enquo(x)
  y <- enquo(y)
  z <- enquo(z)
  a <- enquo(a)
#  output <- dplyr::select(df, !!x, `:`(!!y, !!z))
  df %>% dplyr::select(!!x, `:`(!!y, !!z)) %>%
    dplyr::select(!!a) # this line was to test for pipe operating...can delete really
  # AKA, it should run df %>% select(x, y:z)
#  return(output)
}

# Select columns a and c:e
foo(df,a,c,e,a)
foo(df,a,c,e,b) # should throw an error because b was dropped

fubar <- function(x) {
  output <- str_sub(x,-3,-1)
  str_sub(x,-2,-1)
}
fubar("adam.fw1")


# Function to test gather and stringr function
fawla <- function(df,x,y,z) {
  x <- enquo(x)
  y <- enquo(y)
  z <- enquo(z)
  df %>% tidyr::gather("video",!!x,`:`(!!y,!!z)) %>%
    dplyr::mutate(video = stringr::str_to_upper(video))
}
fawla(df,"num",d,f)

df %>% dplyr::select(!!x, `:`(!!y, !!z)) %>%
  
