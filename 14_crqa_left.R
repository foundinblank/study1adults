## CRQA Optimization for LEFT HAND
# The outcome is another CRQA file but just for the left hand
# Then we should combine both
# And we could test for differences in radius based on a fixed RR of 5 

library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(zoo)
library(crqa)
library(furrr)
library(ggstatsplot)
library(lme4)

# The results I like come from lines 315-355

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



# Get Left Hand Data -----------------------------------------------------
# At around line 600 we combine this with LEFT HAND DATA 
max_per_story <- data_ma %>% 
  group_by(story, name) %>%
  summarise(max_rows = n()) %>%
  group_by(story) %>%
  summarise(max = max(max_rows)) %>%
  spread(story, max)

lhand_files <- list.files("aoi_position",
                          pattern = "left",
                          full.names = T) %>%
  set_names(.)

lhand <- lhand_files %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(sec = col_double(),
                           x = col_double(),
                           y = col_double()
          )) %>%
  mutate(story = str_remove(source, "aoi_position/aoi_"),
         story = str_remove(story, "_left.csv")) %>%
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

bears_lhand <- seq(1, max_per_story$bears) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "bears"), by = "sec") %>%
  fill(story, x, y)

cinderella_lhand <- seq(1, max_per_story$cinderella) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "cinderella"), by = "sec") %>%
  fill(story, x, y)

midas_lhand <- seq(1, max_per_story$midas) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "midas"), by = "sec") %>%
  fill(story, x, y) 

redridinghood_lhand <- seq(1, max_per_story$redridinghood) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "redridinghood"), by = "sec") %>%
  fill(story, x, y)

lhand_all <- bind_rows(bears_lhand,
                       cinderella_lhand,
                       midas_lhand,
                       redridinghood_lhand) %>%
  group_by(story) %>%
  summarise(lhand_y = list(y)) %>%
  mutate(lhand_len = lengths(lhand_y))



# Combine Both Datasets ---------------------------------------------------

# Combine our eye tracking data and left hand data
# We can check lengths to make sure both vectors match for each participant & story

data_lists <- data_ma %>%
  group_by(name, maingroup, story, direction) %>%
  summarise(eye_y = list(y_ma5)) %>%
  mutate(eye_len = lengths(eye_y)) %>%
  left_join(lhand_all, by = "story") %>%
  ungroup()

data_lists %>%
  mutate(samesame = lhand_len == eye_len) %>%
  filter(!samesame)


# Establish CRQA Parameters -----------------------------------------------
# We will work with forward stories only to establish their parameters, as that's the baseline. 
# We will establish 8 sets of parameters, one for each story and direction. 
# Note - I re-ran with moving average = 10 and the parameters barely changed so if we want to experiment with more smoothed data, we dont' need to change the parameters for now

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
# fw bears - (34, 14, 105)
# fw rrh - (27, 10, 120) with filter(name != "Cami")
# fw cinderella - (25, 10, 115)
# fw midas - (22, 10, 117)
# rv bears - (31, 14, 106)
# rv rrh - (28, 9, 120) with filter(name != "Allison")
# rv cinderella - (30, 9, 114)
# rv midas - (20, 10, 118)

# Safely version of optimizeParam
my_optimize_params <- safely(optimizeParam)

plan(multiprocess)
optimize_params <- data_lists %>%
  # If need, add filters here to get rid of phase_space errors (see above)
  # filter(name != "Allison") %>%
  filter(direction == "fw") %>%
  filter(story == "bears") %>%
  # Change optimizeParam to my_optimize_params to catch errors
  mutate(params = future_map2(eye_y, lhand_y, ~ optimizeParam(.x, .y, mlpar)))

# For catching errors ----
# I use my_optimize_params in place of optimizeParam in the above pipe
# to help catch when specific people throw phase-space errors
# if using it, use this to find null rows for filtering those w <smplErrr>
optimize_params %>%
  select(name, params) %>%
  unnest() %>%
  print(n = 50)
# -----

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

# Collect all our parameters (yes, it's manual, deal with it.)
story_parameters_with_lhand <- tribble(
  ~story, ~direction, ~r, ~embeddim, ~delay,
  "bears", "fw", 34, 14, 105,
  "redridinghood", "fw", 27, 10, 120,
  "cinderella", "fw", 25, 10, 115, 
  "midas", "fw", 22, 10, 117,
  "bears", "rv", 31, 14, 106, 
  "redridinghood", "rv", 28, 9, 120,
  "cinderella", "rv", 30, 9, 114,
  "midas", "rv", 20, 10, 118
)


# Optimizing Radius  ------------------------------------------------------
# Per Wallot & Leonardi, we're going to run multiple CRQAs on each participant using 
# the averaged CRQA parameters for embeddim & delay, fixing the target
# %REC (that's RR) at 5.0% and then optimizing the radius. 
# Then we'll run CRQA again using that radius. 

test_radius <- list(seq(5,60))

run_crqa_return_rr <- function(a, b, c, d, e){
  results <- crqa(a, 
                  b, 
                  delay = d, 
                  embed = c, 
                  rescale = 2, 
                  radius = e, 
                  normalize = 2, 
                  mindiagline = 2, 
                  minvertline = 2, 
                  tw = 0, 
                  whiteline = FALSE, 
                  recpt = FALSE, 
                  side = 'both')
  return(results[['RR']])
}

plan(multiprocess)
start_time <- Sys.time()
optimizing_radius <- data_lists %>%
  left_join(story_parameters_with_lhand, by = c("story", "direction")) %>%
  select(-r) %>%
  add_column(test_radius) %>%
  unnest(test_radius, .drop = F) %>%
  group_by(name, story, direction) %>%
  mutate(rr_values = future_pmap_dbl(list(eye_y,
                                          lhand_y,
                                          embeddim,
                                          delay,
                                          test_radius), run_crqa_return_rr))
end_time <- Sys.time()
end_time - start_time
# Time difference of ___ hours

saveRDS(optimizing_radius, "crqa_lhand.RDS")

# optimizing_radius <- readRDS("crqa_lhand.RDS")
target_radius <- optimizing_radius %>%
  select(name, story, direction, test_radius, rr_values) %>%
  mutate(diff = abs(5 - rr_values)) %>%
  group_by(name, story, direction) %>%
  filter(diff == min(diff)) %>%
  ungroup() %>%
  select(name, story, direction, test_radius)

# checking normality 
target_radius %>% ggplot(aes(x = test_radius)) + geom_density()
car::qqPlot(target_radius$test_radius)