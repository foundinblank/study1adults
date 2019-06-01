library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(zoo)
library(crqa)
library(furrr)

# Load All Participants' Eye Tracking Data --------------------------------

files <- list.files("../Adult Data/rawdata/rawdataXLSX", 
                             full.names = T) %>%
  set_names(.)

rawdata <- files %>%
  map_dfr(read_xlsx, .id = "source") %>%
  clean_names() %>%
  rename(x = gaze_point_x_mc_spx,
         y = gaze_point_y_mc_spx,
         language = language_value,
         name = participant_name,
         group = group_value) %>%
  select(source, name, gaze_point_index, x, y)

# Fix for stories that don't start at gaze_point_index = 1
rawdata <- rawdata %>%
  group_by(source, name) %>%
  arrange(source, name, gaze_point_index) %>%
  mutate(gaze_point_index = row_number()) %>%
  ungroup()

# Extract story and direction
rawdata <- rawdata %>%
  mutate(source = str_remove(source, "../Adult Data/rawdata/rawdataXLSX/"),
         source = str_remove(source, ".xlsx")) %>%
  mutate(direction = str_sub(source, -2, -1)) %>%
  mutate(story = str_remove(source, "fw"),
         story = str_remove(story, "rv")) %>%
  select(-source)

# Pull clean names (and handle Laura2 for Laura (missing stories))
cleannames <- read_csv("partnames.csv") %>%
  distinct() %>%
  rename(name = participant) %>%
  filter(name != "Laura2")

# Pull final group assignments
cleangroups <- read_csv("finaldataset.csv") %>%
  select(participant, maingroup) %>%
  rename(name = participant) %>%
  distinct()

# Join both to rawdata
data <- rawdata %>%
  left_join(cleannames, by = "name") %>%
  select(-name) %>%
  rename(name = new_participant) %>%
  filter(!is.na(name)) %>%
  left_join(cleangroups, by = "name")

# Exclude those who weren't included in previous analyses (find out why not)
# Also exclude 4 stories (Sarah & Josh) because theirs were too long 
# (Find out how we dealt with those before)
# And exclude more because they have no data...???
excluded <- read_csv("finaldataset.csv") %>%
  select(participant, story, direction, eye_exclude) %>%
  rename(name = participant) %>%
  mutate(story = tolower(story)) %>%
  mutate(story = case_when(
    story == "goldilocks" ~ "bears",
    story == "kingmidas" ~ "midas",
    TRUE ~ story
  )) %>%
  mutate(direction = case_when(
    direction == "forward" ~ "fw",
    direction == "reversed" ~ "rv"
  )) %>%
  # Excluded because too much data?
  mutate(eye_exclude = case_when(
    name == "Sara" & story == "cinderella" & direction == "rv" ~ TRUE,
    name == "Josh" & story == "redridinghood" & direction == "fw" ~ TRUE,
    name == "Sara" & story == "midas" & direction == "fw" ~ TRUE,
    name == "Josh" & story == "midas" & direction == "rv" ~ TRUE,
    TRUE ~ eye_exclude
  )) %>%
  # Excluded because they have no data?
  mutate(eye_exclude = case_when(
    name == "Amy" & story == "midas" & direction == "fw" ~ TRUE,
    name == "Amy" & story == "redridinghood" & direction == "rv" ~ TRUE,
    name == "Laura (missing stories)" & story == "midas" & direction == "fw" ~ TRUE,
    TRUE ~ eye_exclude
  )) %>%
  # Excluded because they still had too much data?
  mutate(eye_exclude = case_when(
    name == "Alicia" & story == "midas" ~ TRUE,
    name == "Alicia" & story == "redridinghood" ~ TRUE,
    TRUE ~ eye_exclude
  ))

data <- data %>% 
  left_join(excluded, by = c("name", "story", "direction")) %>%
  filter(!eye_exclude) %>%
  filter(!is.na(eye_exclude)) %>%
  select(-eye_exclude) %>%
  mutate_at(vars(name, maingroup, story, direction), as.factor)

# Counting
data %>% 
  distinct(name, maingroup) %>% 
  count(maingroup) %>% 
  rename(participants = n)

data %>%
  distinct(name, maingroup, story, direction) %>% 
  count(maingroup, story, direction) %>%
  spread(direction, n)


# Data Smoothing ----------------------------------------------------------
# First, fill in missing values with a straightforward fill()
# We're adding smoothing with a moving average window size - 5. (y_ma5). Good for removing noise. Then we'll eliminate 30 samples from the start and end (that's 0.25 seconds on both sides). 


bookend <- 30
max_end <- data %>% 
  group_by(story, maingroup) %>%
  summarise(max = max(gaze_point_index)) %>%
  group_by(story) %>%
  summarise(max_end = min(max) - bookend)

data_ma <- data %>%
  group_by(name, story, direction) %>%
  fill(y, .direction = "down") %>%
  fill(y, .direction = "up") %>%
  mutate(y_ma5 = rollmean(y, 5, fill = NA)) %>%
  ungroup() %>%
  left_join(max_end, by = "story") %>%
  filter(gaze_point_index > bookend & gaze_point_index < max_end) %>%
  mutate(gaze_point_index = gaze_point_index - bookend)

# data_ma %>%
#   filter(name == "Monica",
#          story == "bears") %>%
#   ungroup() %>%
#   gather(key = "metric", value = "y_position", c(y, y_ma5)) %>%
#   ggplot(aes(x = gaze_point_index, y = y_position, color = metric)) +
#   geom_line(alpha = 0.5) +
#   scale_y_reverse() +
#   ggtitle("Monica - see effect of smoothing with MA = 5")

  

# Get Right Hand Data -----------------------------------------------------
max_per_story <- data_ma %>% 
  group_by(story, name) %>%
  summarise(max_rows = n()) %>%
  group_by(story) %>%
  summarise(max = max(max_rows)) %>%
  spread(story, max)

rhand_files <- list.files("aoi_position",
                          pattern = "right",
                          full.names = T) %>%
  set_names(.)

rhand <- rhand_files %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(sec = col_double(),
                           x = col_double(),
                           y = col_double()
          )) %>%
  mutate(story = str_remove(source, "aoi_position/aoi_"),
         story = str_remove(story, "_right.csv")) %>%
  select(-source) %>%
  mutate(sec = floor(sec*120)) %>%
  mutate(sec = sec + 1) %>%
  add_row(story = "bears", sec = max_per_story$bears) %>%
  add_row(story = "redridinghood", sec = max_per_story$redridinghood) %>%
  add_row(story = "midas", sec = max_per_story$midas) %>%
  add_row(story = "cinderella", sec = max_per_story$cinderella) %>%
  arrange(story, sec) %>%
  group_by(story) %>%
  fill(x, y) %>%
  # Fix duplicate gaze_point_index 
  group_by(story, sec) %>%
  slice(1)

bears_rhand <- seq(1, max_per_story$bears) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "bears"), by = "sec") %>%
  fill(story, x, y)
  
cinderella_rhand <- seq(1, max_per_story$cinderella) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "cinderella"), by = "sec") %>%
  fill(story, x, y)

midas_rhand <- seq(1, max_per_story$midas) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "midas"), by = "sec") %>%
  fill(story, x, y) 

redridinghood_rhand <- seq(1, max_per_story$redridinghood) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "redridinghood"), by = "sec") %>%
  fill(story, x, y)

rhand_all <- bind_rows(bears_rhand,
                       cinderella_rhand,
                       midas_rhand,
                       redridinghood_rhand) %>%
  group_by(story) %>%
  summarise(rhand_y = list(y)) %>%
  mutate(rhand_len = lengths(rhand_y))



# Combine Both Datasets ---------------------------------------------------

# Combine our eye tracking data and right hand data
# We can check lengths to make sure both vectors match for each participant & story

data_lists <- data_ma %>%
  group_by(name, maingroup, story, direction) %>%
  summarise(eye_y = list(y_ma5)) %>%
  mutate(eye_len = lengths(eye_y)) %>%
  left_join(rhand_all, by = "story") %>%
  ungroup()

data_lists %>%
  mutate(samesame = rhand_len == eye_len) %>%
  filter(!samesame)


# Establish CRQA Parameters -----------------------------------------------
# We will work with forward stories only to establish their parameters, as that's the baseline. 
# We will establish 4 sets of parameters, one for each story. 

mlpar <- list(lgM = 120, 
              radiusspan = 100, 
              radiussample = 10, 
              normalize = 2, 
              rescale = 2, 
              mindiagline = 2, 
              minvertline = 2, 
              tw = 0, 
              whiteline = FALSE, 
              recpt = FALSE, 
              fnnpercent = 10, 
              typeami = "maxlag")

# We might have issues so let's go through each of the 8 batches one by one
# fw bears - ok (34, 14, 117)
# fw rrh - phase-space
# fw cinderella - ok (28, 14, 110)
# fw midas - ok (20, 10, 120) - took a long time
# rv bears - ok (19, 14, 117)
# rv rrh - phase-space
# rv cinderella (32, 13, 104) with filter(name != "Valerie" & name != "Jesse")
# rv midas - ok (18, 10, 120)


plan(multiprocess)
optimize_params <- data_lists %>%
  filter(direction == "rv") %>%
  filter(story == "redridinghood") %>%
  mutate(params = future_map2(eye_y, rhand_y, ~ optimizeParam(.x, .y, mlpar)))

output_params <- optimize_params %>%
  group_by(name) %>%
  mutate(r = pluck(params, 1, 1),
         dim = pluck(params, 1, 2),
         delay = pluck(params, 1, 3))

param_means <- output_params %>%
  ungroup() %>%
  summarise(r_mean = ceiling(mean(r)),
            dim_mean = ceiling(mean(dim)),
            delay_mean = ceiling(mean(delay)))

param_means
