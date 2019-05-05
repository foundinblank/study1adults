## Reads in AOI position text files from Toii
## Will return tibble of columns sec, x, y, with rows for each timestamp/AOI position

get_xy_timestamp <- function(z){

x <- read_lines(z) %>%
  as_tibble() %>%
  rowid_to_column("ID")

# pull out timestamp rows
timestamps <- x %>%
  filter(str_detect(value, "timestamp")) %>%
  separate(value, into = c("throw","timestamp"), sep = " = ") %>%
  separate(timestamp, into = c("one","two","sec"), sep = ":") %>%
  select(sec) %>%
  mutate(sec = as.numeric(sec))

# Find AOI coordinate rows
xys <- x %>%
  mutate(find = str_detect(value, "X\tY"))

# first vertex (2 rows after each "XY" line)
xys_row <- which(xys$find) + 2

xy1 <- x %>%
  filter(ID %in% xys_row) %>%
  separate(value, into = c("x1","y1"), sep = "\t") %>%
  select(-ID)

# opposite vertex (2 more rows down)
xys_row <- xys_row + 2

xy2 <- x %>%
  filter(ID %in% xys_row) %>%
  separate(value, into = c("x2","y2"), sep = "\t") %>%
  select(-ID)

# Now get average XY position per timestamp
xys <- cbind(timestamps, xy1, xy2) %>%
  mutate_all(as.numeric) %>%
  rowwise() %>%
  mutate(x = mean(x1,x2),
         y = mean(y1, y2)) %>%
  select(sec, x, y)

xys
}
